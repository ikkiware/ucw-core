;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; ** STANDARD-SERVER

(defprint-object (server standard-server)
  (format *standard-output* "~S ~S" (type-of (server.backend server)) (length (server.applications server))))

(defmethod startup-server :around ((server standard-server))
  (with-lock-held-on-server server
    (call-next-method)))

(defmethod shutdown-server :around ((server standard-server))
  (with-lock-held-on-server server
    (call-next-method)))

(defmethod register-application :around ((server standard-server) (app application))
  (with-lock-held-on-server server
    (call-next-method)))

(defmethod unregister-application :around ((server standard-server) (app application))
  (with-lock-held-on-server server
    (call-next-method)))

(defmethod startup-server ((server standard-server))
  "Startup SERVER. This calls startup-backend on the server's
backend and startup-application on all the application objcets
registered with SERVER."
  (ucw.rerl.server.debug "Starting up standard-server ~A" server)
  (let ((backend (server.backend server)))
    (initialize-backend backend :server server)
    (startup-backend backend)
    (setf (server.started server) t)
    (ucw.rerl.server.info "Started standard-server ~A with backend ~A" server backend)
    (dolist (app (server.applications server))
      (startup-application app))))

(defmethod shutdown-server ((server standard-server))
  "First call SHUTDOWN-APPLICATION on all the apps registered
with SERVER, then call SHUTDOWN-BACKEND on SERVER's backend."
  (ucw.rerl.server.debug "Shutting down standard-server ~A" server)
  (let ((backend (server.backend server)))
    (dolist (app (server.applications server))
      (shutdown-application app))
    (ucw.backend.debug "Shutting down backend ~A" backend)
    (shutdown-backend backend)
    (setf (server.started server) nil))
  (ucw.rerl.server.info "Finished shutting down standard-server ~A" server))

(defmethod restart-server ((server standard-server))
  (ucw.rerl.server.info "Restarting standard-server ~A" server)
  (when (server.started server)
    (shutdown-server server))
  (startup-server server))

(defmethod register-application ((server standard-server) (app application))
  (setf (server.applications server)
        (delete app (server.applications server) :test #'eq))
  (setf (server.applications server)
        (sort (cons app (server.applications server)) #'>
              :key (lambda (app) (length (application.url-prefix app)))))
  (setf (application.server app) server))

(defmethod unregister-application ((server standard-server) (app application))
  (setf (server.applications server) (delete app (server.applications server) :test #'eq)))

(defmethod find-associated-application ((server standard-server) (request request))
  (find (query-path request)
        (server.applications server)
        :test #'starts-with :key #'application.url-prefix))

(defun abort-request (&optional (why nil why-p))
  (ucw.rerl.application.info "Gracefully aborting request~:[.~; because ~S.~]" why-p why)
  (invoke-restart (find-restart 'abort-request)))

(defun call-as-request-handler (server request response thunk)
  "Sets up a proper request-handling environment around THUNK"
  (restart-case
       ;; we need a RESTART-BIND here to see the *CURRENT-CONDITION* binding
       (restart-bind
            ((generate-backtrace-for-emacs
              (lambda ()
                (send-backtrace-to-emacs server
                                         *current-condition*
                                         (collect-backtrace *current-condition*))
                (send-standard-error-page :condition *current-condition*)
                (return-from call-as-request-handler))
              :report-function (lambda (stream)
                                 (format stream "Generate a bug report in Emacs and send an internal server error page."))))
         (funcall thunk))   ; its return value is important!
    (retry-handling-the-request ()
      :report "Restart the HANDLE-REQUEST protocol at the server level"
      (clear-response response)
      (handle-request server request response))
    (abort-request ()
      :report "Abort processing this request at the server level"
      (values))))

(defun call-as-response-handler (response thunk &key (content-type "text/html") send-response)
  (setf (get-header response "Status") +http-ok+
        (get-header response "Content-Type") content-type
        (get-header response "Date") (date:universal-time-to-http-date
                                      (get-universal-time)))
  (multiple-value-prog1
      (with-yaclml-stream (html-stream response)
        (funcall thunk))
    (when send-response
      (send-response response))))

(defmethod handle-request ((server standard-server)
                           (request request)
                           (response response))
  (call-as-request-handler
   server request response
   (lambda ()
     ;; find the application which may handle this request
     (when-bind application (find-associated-application server request)
       (let ((*context* (make-request-context application request response)))
         (ucw.rerl.server.dribble "Application ~A handling request with context ~A." application *context*)
         (block handling
           (call-with-ucw-error-handler
            (lambda ()
              (service application *context*))
            (lambda (error)
              (ignore-errors
                (ucw.rerl.server.debug "Application ~A captured a request error: ~A"
                                       application error))
              (handle-toplevel-condition application error nil)
              (return-from handling)))))))))

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
