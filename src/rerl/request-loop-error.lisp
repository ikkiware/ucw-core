;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; ** Error handling

;;;; *** Our simple wrapper around slime's backtrace lists.

(defclass stack-frame ()
  ((description :initarg :description :accessor description-of)
   (local-variables :initarg :local-variables :accessor local-variables-of)
   (source-location :initarg :source-location :accessor source-location-of)))

(defun make-stack-frame (description &optional source-location local-variables)
  (make-instance 'stack-frame
                 :description description
                 :source-location source-location
                 :local-variables local-variables))

(defun collect-backtrace (condition &key (start 4) (end 500))
  (let ((swank::*swank-debugger-condition* condition)
        (swank::*buffer-package* *package*))
    (swank-backend:call-with-debugging-environment
     (lambda ()
       (iter (for (index description) :in (swank:backtrace start end))
             (collect (make-stack-frame description
                                        (ignore-errors
                                          (if (numberp index)
                                              (swank-backend:frame-source-location index)
                                              index))
                                        (swank-backend:frame-locals index))))))))

;;;; *** Handling internal UCW errors


;; TODO the functionality here may be worth reviving in the new error handling
#+nil(defmethod handle-request-error ((error error) backtrace)
  (if (and (boundp '*context*)
           (context.session *context*)
           (context.current-frame *context*))
      (progn
        (setf (frame.window-component (context.current-frame *context*))
              (make-instance 'error-component :condition error
                             :message (princ-to-string error)
                             :backtrace backtrace))
        (with-yaclml-stream (html-stream (context.response *context*))
          (render (frame.window-component (context.current-frame *context*)))))
      (with-error-page (:title "An internal server error has occured.")
        (<:p "An internal server error has occured.")
        (block walk-backtrack
          (handler-bind ((error (lambda (c)
                                  (return-from walk-backtrack c))))
            (<:table 
                     (<:tr (<:th "Index") (<:th "Description") (<:th "Locals") (<:th "Source"))
                     (dolist (b backtrace)
                       (<:tr (<:td (<:as-html (index-of b)))
                             (<:td (<:as-html (description-of b)))
                             (<:td (<:as-html (local-variables-of b)))
                             (<:td (<:as-html (source-location-of b)))))))))))

;;;; *** Handling user errors

(defvar *current-condition*)

(defun call-with-ucw-error-handler (body error-handler)
  (handler-bind
      ((serious-condition
        (lambda (error)
          (with-thread-name " / CALL-WITH-UCW-ERROR-HANDLER"
            (let* ((parent-condition (when (boundp '*current-condition*)
                                       *current-condition*))
                   (*current-condition* error))
              (if parent-condition
                  (let ((error-message (or (ignore-errors
                                             (format nil "Nested error while handling error: ~A, the second error is ~A"
                                                     parent-condition error))
                                           (ignore-errors
                                             (format nil "Failed to log nested error message, probably due to nested print errors. Condition type is ~S."
                                                     (type-of error)))
                                           "Failed to log nested error message, probably due to nested print errors.")))
                    (ignore-errors
                      (ucw.rerl.error error-message))
                    ;; let the error fall through and most probably reach the toplevel debugger if left enabled.
                    ;; there's really nothing else we could do here, because silently aborting the request
                    ;; and pretending that nothing bad happened could lead to server hangs, especially with
                    ;; stack overflow errors. entering the toplevel debugger or exiting with an error code is
                    ;; still better then a silent hang...
                    )
                  (progn
                    (if (and (typep error 'stream-error)
                             (boundp '*request*)
                             (eq (stream-error-stream error)
                                 (network-stream *request*)))
                        (ucw.rerl.debug "Ignoring stream error coming from the network stream:~%~A" error)
                        (progn
                          (ucw.rerl.debug "Calling custom error handler from CALL-WITH-UCW-ERROR-HANDLER for error: ~A" error)
                          (funcall error-handler error)))
                    (abort-backend-request error)
                    (assert nil nil "Impossible code path in CALL-WITH-UCW-ERROR-HANDLER"))))))))
    (funcall body)))

(defun send-standard-error-page (&key condition (message "An internal server error has occured." message-p)
                                      (title message) (http-status-code +http-internal-server-error+))
  (ucw.rerl.info "Sending ~A for request ~S" http-status-code (if (boundp '*request*)
                                                                  (query-path *request*)
                                                                  "?"))
  (when (and (not message-p)
             condition)
    (ignore-errors
      (setf message (with-output-to-string (str)
                      (princ condition str)))))
  (if (boundp '*response*)
      (if (headers-are-sent-p *response*)
          (ucw.rerl.error "The headers are already sent, closing the socket as-is without writing any useful error message")
          (render-standard-error-page :message message :title title
                                      :http-status-code http-status-code))
      ;; TODO we should be smarter then that: signalling due to *request-content-length-limit* happens without a *response*
      ;; but *response* stuff could also be fixed: why is *response* created after *request* and why is it an arg of make-response?
      (ucw.rerl.error "Hm, even *response* is unbound, just closing the socket without any useful error message")))

(defun render-standard-error-page (&key (message "An internal server error has occured.")
                                        (title message) (http-status-code +http-internal-server-error+))
  (clear-response *response*)
  (handle-raw-request (:content-type "text/html" :with-yaclml-stream t
                       :http-status-code http-status-code
                       :error-handler nil)
    ;; TODO render some token that can be used to search the logs?
    (<:html
     (<:head (<:title (<:as-html title)))
     (<:body (<:h1 (<:as-html title))
             (<:p (<:as-html message))))))

(defun invoke-slime-debugger-if-possible (condition)
  (if (or swank::*emacs-connection*
          (swank::default-connection))
      (progn
        (ucw.rerl.debug "Invoking swank debugger for condition: ~A" condition)
        (swank:swank-debugger-hook condition nil))
      (ucw.rerl.debug "No Swank, not debugging error:~%~A" condition)))

(defmethod handle-toplevel-condition :before (application (error serious-condition) action)
  (when (and (debug-on-error application)
             (not (typep error 'no-handler-for-request)))
    (restart-case
         (invoke-slime-debugger-if-possible error)
      (continue ()
        :report "Continue processing the error (and probably send an error page)"
        (return-from handle-toplevel-condition)))))

;;; Not exporting for now (name sucks, not sure about behavior)
(defvar *ucw-backtrace-verbose* nil
  "When `t' display local variables in error log backtrace")

(defun log-error-with-backtrace (error)
  (ucw.rerl.error "~%*** At: ~A~%*** In thread: ~A~%*** Error:~%~A~%*** Backtrace is:~%~A"
                  (format-rfc3339-timestring nil (now))
                  (thread-name (current-thread))
                  error
                  (let ((backtrace (collect-backtrace error))
                        (*print-pretty* nil))
                    (with-output-to-string (str)
                      (iter (for stack-frame :in backtrace)
                            (for index :upfrom 0)
                            (format str "~3,'0D: " index)
                            (write-string (description-of stack-frame) str)
			    (when-bind locals (and *ucw-backtrace-verbose*
						   (local-variables-of stack-frame))
			      (format str "~&~TFrame local variables~&")
			      (iter (for local-var :in locals)
				    (format str "~&~2T~A: ~S~&"
					    (getf local-var :name)
					    (getf local-var :value))))
                            (format str "~&"))))))

(defmethod handle-toplevel-condition (application (error serious-condition) action)
  (ignore-errors (log-error-with-backtrace error))
  (send-standard-error-page :condition error)
  (abort-backend-request error))

(defmethod handle-toplevel-condition (application (error no-handler-for-request) action)
  (send-standard-error-page :condition error :http-status-code +http-not-found+
                            :title "The page you requested was not found"
                            :message (format nil "~S was not found on this server" (raw-uri-of error)))
  (abort-backend-request "Page was not found"))

(defmethod handle-toplevel-condition (application (error too-many-sessions) action)
  (ucw.rerl.info "Sending error page due to too-many-sessions error")
  (send-standard-error-page :condition error :http-status-code +http-service-unavailable+
                            :title "Too many live sessions"
                            :message "The server is overloaded, please try again later")
  (abort-backend-request "Too many sessions"))

;;;; *** Generating bug reports in emacs

(defun send-backtrace-to-emacs (server condition backtrace)
  (ucw.debug "Sending backtrace to emacs from condition ~A" condition)
  (let ((swank::*emacs-connection* (or swank::*emacs-connection*
                                       (swank::default-connection))))
    (let ((backtrace (generate-backtrace-for-emacs server condition backtrace)))
      (swank::eval-in-emacs
       `(save-excursion
         (loop
             with buffer-name = "*UCW Backtrace <%d>*"
             for id upfrom 0
             for backtrace-buffer = (get-buffer (format buffer-name id))
             while backtrace-buffer
             finally do (switch-to-buffer-other-window (format buffer-name id)))
         (insert ,backtrace))))))

;; TODO this is badly broken due to varuous escaping issues when emacs is read'ing the message
(defun generate-backtrace-for-emacs (server condition backtrace)
  (assert (and server condition backtrace))
  (let ((*print-circle* t)
        (*print-pretty* nil)
        (*print-readably* nil)
        (*print-level* nil)
        (*print-length* nil))
    ;; to escape inner quotes
    (with-output-to-string (s)
      (flet ((show-obj (label object)
               (format s "~A: ~S~%" label object)
               (describe object s)))
        (write-line   "--- UCW Backtrace" s)
        (show-obj     "---    Condition" condition)
        (multiple-value-bind (second minute hour date month year )
            (decode-universal-time (get-universal-time))
          (format s   "---    Date: ~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~%"
                  year month date hour minute second))
        (format s     "---    Lisp: ~S ~S~%" (lisp-implementation-type) (lisp-implementation-version))
        (show-obj     "---    Server" server)
        (show-obj     "---    Backend" (server.backend server))
        (when (boundp '*context*)
          (show-obj     "---    Application" (context.application *context*))
          (show-obj     "---    Request" (context.request *context*))
          (show-obj     "---    Response" (context.response *context*)))
        (write-line   "--- BACKTRACE" s)
        (dolist (frame backtrace)
          (format s   "--- FRAME ~D~%" (index-of frame))
          (write-line (description-of frame) s)
          (write-line "---   Locals:" s)
          (dolist (local (local-variables-of frame))
            (format s "~S ==> ~S~%" (getf local :name) (getf local :value)))
          (write-line "---   Source:" s)
          (format s "~S~%" (source-location-of frame)))))))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
