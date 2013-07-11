;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defclass basic-backend (backend)
  ((host :accessor host :initarg :host :initform nil)
   (port :accessor port :initarg :port :initform nil)
   (socket :initform nil :accessor socket)
   (server :accessor server :initarg :server)
   (handlers :accessor handlers :initform '())
   (request-content-length-limit :initform *request-content-length-limit*
                                 :accessor request-content-length-limit-of
                                 :initarg :request-content-length-limit)))

(defprint-object (self basic-backend)
  (write-string ":host ")
  (princ (host self))
  (write-string " :port ")
  (princ (port self)))

(defclass basic-message (message)
  ((headers :accessor headers :initform '())
   (socket :accessor socket :initarg :socket)))

(defclass basic-request (basic-message request)
  ((cookies :accessor cookies)
   (parameters :accessor parameters :initform '())
   (raw-uri :accessor raw-uri :initform nil)
   (query-path :accessor query-path :initform nil)
   (raw-body :accessor raw-body :initform nil)
   (http-method :accessor http-method :initform nil)))

(defclass basic-response (basic-message response)
  ((headers-are-sent :accessor headers-are-sent-p :initform nil :type boolean)
   (cookies :accessor cookies :initform '())
   (request :accessor request :initarg :request :initform nil)
   (html-stream :accessor html-stream :initform (make-string-output-stream))
   (status :accessor status :initform +http-ok+)
   (external-format :accessor external-format :initform nil)
   (content :accessor content :initform nil)))

(defclass lockable-backend-mixin ()
  ((lock
    :initform (make-recursive-lock "backend lock")
    :accessor lock-of)))

(defmacro with-lock-held-on-backend (backend &body body)
  `(with-recursive-lock-held ((lock-of ,backend))
    ,@body))


;;;; Cookies

(defmethod cookies ((request basic-request))
  (if (slot-boundp request 'cookies)
      (slot-value request 'cookies)
      (setf (slot-value request 'cookies)
            ;; TODO consider calling safe-parse-cookies, see rfc2109 comments
            (rfc2109:parse-cookies (get-header request "Cookie")))))

(defmethod find-cookie ((request basic-request) cookie)
  (find-cookie-using-request (context.request *context*) cookie))

(defmethod find-cookie-using-request ((request basic-request) cookie)
  (let ((cookie-name (cond ((stringp cookie) cookie)
                           ((rfc2109:cookie-p cookie) (rfc2109:cookie-name cookie))
                           (t (error "FIND-COOKIE only supports string and rfc2109:cookie struct as cookie name specifier")))))
    (find cookie-name (cookies request) :test #'string= :key #'rfc2109:cookie-name)))

(defun cookie-value (cookie &optional default)
  (cookie-value-using-request (context.request *context*) cookie default))

(defmethod cookie-value-using-request ((request basic-request) cookie &optional default)
  (aif (find-cookie request cookie)
       (unescape-as-uri (rfc2109:cookie-value it))
       default))

(defun add-cookie (cookie)
  "Add cookie to the current response."
  (add-cookie-using-response (context.response *context*) cookie))

(defmethod add-cookie-using-response ((response basic-response) cookie)
  (assert (rfc2109:cookie-p cookie))
  (push cookie (cookies response)))


;;;; Backend methods
(defmethod initialize-backend ((backend basic-backend) &key server &allow-other-keys)
  (when (and (null *mime-types*)
             (probe-file *default-mime-types-file*))
    (read-mime-types *default-mime-types-file*))
  (setf (server backend) server)
  backend)

(defmethod handle-request ((backend basic-backend) (request basic-request) (response basic-response))
  (let ((start-time (get-internal-real-time))
        (remote-address (remote-address request))
        (raw-uri (raw-uri request)))
    (ucw.backend.info "Handling request from ~S for ~S" remote-address raw-uri)
    (or (block handle
          (dolist* ((can-match handler url-base) (handlers backend))
            (declare (ignore url-base))
            (when (funcall can-match (query-path request))
              (funcall handler request response)
              (return-from handle t)))
          nil)
        (handle-request (server backend) request response)
        (error 'no-handler-for-request :raw-uri raw-uri :request request))
    (let ((seconds (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
      (when (> seconds 0.05)
        (ucw.backend.info "Handled request in ~,3f secs (request came from ~S for ~S)"
                          seconds remote-address raw-uri)))))

(defmethod publish-directory ((backend basic-backend) directory-pathname url-base)
  (push (list (lambda (request-url)
                (ucw.backend.dribble "Trying to match '~S' under url-base '~S' to serve it as a file from '~S'"
                                     request-url url-base directory-pathname)
                (starts-with request-url url-base))
              (lambda (request response)
                (aif (map-query-path-to-file (query-path request)
                                             url-base
                                             directory-pathname)
                     (progn
                       (ucw.backend.debug "Serving [~S] as a file under url-base [~S]" it url-base)
                       (serve-file it :request request :response response))
                     (progn
                       (ucw.backend.debug "Failed to serve [~S] as a file under url-base [~S]" (query-path request) url-base)
                       (error 'no-handler-for-request :raw-uri (raw-uri request) :request request))))
              url-base)
        (handlers backend)))

;;;; Message headers methods

(defmethod get-header ((message basic-message) header-name)
  (cdr (assoc header-name (headers message) :test #'string-equal)))

(defmethod (setf get-header) (value (message basic-message) header-name)
  (aif (assoc header-name (headers message) :test #'string-equal)
       (setf (cdr it) value)
       (push (cons header-name value) (headers message)))
  value)

(defmethod add-header ((message basic-message) header-name value)
  (push (cons header-name value) (headers message))
  value)

(defmethod delete-header ((message basic-message) header-name)
  (setf (headers message)
        (delete-if #'(lambda (item)
                       (string-equal (car item)
                                      header-name))
                   (headers message))))

(defmethod remote-address :around ((message basic-message))
  (declare (optimize speed)
           (inline localhost-ip-address-p ip-address-from-private-network-p))
  (let ((physical-remote-address (call-next-method)))
    (if (and physical-remote-address
             (or (ip-address-from-private-network-p physical-remote-address)
                 (localhost-ip-address-p physical-remote-address)))
        ;; check if we are in a proxy setup and extract the real remote address if provided.
        ;; but do so only if the physical remote address is coming from a machine from the local net.
        ;; please note that this is not a realiable source for ip addresses!
        (let ((ip-as-string (get-header message "X-Forwarded-For")))
          (when ip-as-string
            (let* ((real-remote-address (first (cl-ppcre:split "," ip-as-string :sharedp t)))
                   (pieces (cl-ppcre:split "\\." real-remote-address :sharedp t)))
              (declare (type list pieces))
              (if (= (length pieces) 4)
                  (iter (with result = (make-array 4 :element-type '(unsigned-byte 8)))
                        (for idx :from 0 :below 4)
                        (for ip-address-part = (parse-integer (pop pieces)))
                        (assert (<= 0 ip-address-part 255))
                        (setf (aref result idx) ip-address-part)
                        (finally (return result)))
                  (progn
                    (ucw.backend.info "Returning NIL instead of an invalid ip address: ~S" ip-as-string)
                    nil)))))
        physical-remote-address)))

;;;; Request handling

(defun read-line-from-network (stream &optional (eof-error-p t))
  "A simple state machine which reads chars from STREAM until it
  gets a CR-LF sequence or the end of the stream."
  (declare (optimize (speed 3)))
  (let ((buffer (make-array 50
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (labels ((read-next-char ()
               (let ((byte (read-byte stream eof-error-p stream)))
                 (if (eq stream byte)
                     (return-from read-line-from-network buffer)
                     (return-from read-next-char byte))))
             (cr ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+linefeed+ ;; LF
                      (return-from read-line-from-network buffer))
                   (t ;; add both the cr and this char to the buffer
                    (vector-push-extend #.+carriage-return+ buffer)
                    (vector-push-extend next-byte buffer)
                    (next)))))
             (next ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+carriage-return+ ;; CR
                      (cr))
                   (#.+linefeed+ ;; LF
                      (return-from read-line-from-network buffer))
                   (t
                    (vector-push-extend next-byte buffer)
                    (next))))))
      (next))))

(defun accumulate-parameters (assoc-list)
  "Accumulates same parameters into lists. Otherwise
  multiple-selection lists won't have a list value and
  <ucw:select would fail."
  (let ((result '()))
    (dolist* ((name . value) assoc-list)
      (unless (string= name "")
        (aif (assoc name result :test #'string=)
             (if (and (cdr it) (listp (cdr it)))
                 (setf (cdr it) (cons value (cdr it)))
                 (setf (cdr it) (list value (cdr it))))
             (push (cons name value) result))))
;;; reverse the (cdr it) so that writer lambda's see the values
;;; in correct order. 
    (dolist (it result)
      (when (and (cdr it) (listp (cdr it)))
        (setf (cdr it) (nreverse (cdr it)))))
;;; rever the result so that map-parameters see the request
;;; parameters in correct order.
    (nreverse result)))

(defgeneric read-request (backend socket)
  (:method :around (backend socket)
    (with-thread-name " / READ-REQUEST"
      (call-next-method)))
  (:method :around ((backend basic-backend) socket)
    (let ((*request-content-length-limit* (request-content-length-limit-of backend)))
      (call-next-method))))

(defun read-basic-request ()
  (let* ((request *request*)
         (stream (network-stream request))
         (line (read-line-from-network stream))
         (pieces (split-on-space line)))
    (ucw.backend.dribble "In read-basic-request, first line in :us-ascii is ~S, pieces are ~S"
                         (ignore-errors
                           (octets-to-string line :us-ascii)) pieces)
    (destructuring-bind (http-method uri &optional protocol) pieces
      (declare (ignore protocol))
      ;; uri's must be foo%12%34bar encoded utf-8 strings in us-ascii. processing anything else here would be ad-hoc...
      (setf (raw-uri request) (coerce (octets-to-string uri :us-ascii) 'simple-string)
            (http-method request) (coerce (octets-to-string http-method :us-ascii) 'simple-string)
            (headers request) (read-request-headers stream))
      (ucw.backend.dribble "Request headers are ~S" (headers request))
      (aif (position #\? (raw-uri request))
           (setf (query-path request) (make-displaced-array (raw-uri request) 0 it)
                 (parameters request) (parse-query-parameters
                                       (make-displaced-array (raw-uri request)
                                                             (1+ it))))
           (setf (query-path request) (raw-uri request)
                 (parameters request) '()))
      (setf (query-path request) (unescape-as-uri (query-path request)))
      (setf (parameters request) (append (parameters request)
                                         (accumulate-parameters
                                          (parse-request-body stream
                                                              (get-header request "Content-Length")
                                                              (get-header request "Content-Type"))))))
    request))

(defun find-parameter (request name &key (test #'string=))
  (loop
     with result = '()
     for (k . v) in (parameters request)
     when (funcall test k name)
       do (if result
              (if (consp result)
                  (push v result)
                  (setf result (list v result)))
              (setf result v))
     finally (return result)))
  
  (defmethod get-parameter ((request basic-request) name)
  (find-parameter request name))

(defmethod map-parameters ((request basic-request) lambda)
  (dolist* ((name . value) (parameters request))
    (unless (string= name "")
      (funcall lambda name (if (stringp value)
                               (copy-seq value)
                               value)))))

(defun read-request-headers (stream)
  (iterate
    (for header-line = (read-line-from-network stream))
    (until (= 0 (length header-line)))
    (for (name . value) = (split-header-line header-line))
    (collect (cons (octets-to-string name :us-ascii)
                   (octets-to-string value :iso-8859-1)))))

(defmethod close-request ((request basic-request))
  request)

;;;; Response objects

(defmethod clear-response ((response basic-response))
  (setf (html-stream response) (make-string-output-stream)
        (headers response) '()))

;;;; basic-response objects special case the "Status" header.

(defmethod get-header ((response basic-response) header-name)
  (if (string= "Status" header-name)
      (status response)
      (call-next-method)))

(defmethod (setf get-header) (value (response basic-response) header-name)
  (if (string= "Status" header-name)
      (setf (status response) value)
      (call-next-method)))

(defun write-crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-header-line (name value stream)
  (write-sequence (string-to-octets name :us-ascii) stream)
  ;; ": "
  (write-byte 58 stream)
  (write-byte 32 stream)
  (write-sequence (string-to-octets value :iso-8859-1) stream)
  (write-crlf stream))

(defmethod encoding ((response basic-response))
  (or (external-format response)
      (call-next-method)))

(defmethod send-headers :before ((response basic-response))
  (assert (not (headers-are-sent-p response)))
  (setf (headers-are-sent-p response) t))

(defmethod send-headers ((response basic-response))
  (ucw.backend.dribble "Sending headers for ~S (Status: ~S)." response (status response))
  (let ((stream (network-stream response)))
    (write-sequence #.(string-to-octets "HTTP/1.1 " :us-ascii) stream)
    (write-sequence (string-to-octets (status response) :us-ascii) stream)
    (write-byte #.(char-code #\Space) stream)
    (write-crlf stream)
    (dolist* ((name . value) (headers response))
      (unless (null value)
        (ucw.backend.dribble "Sending header ~S: ~S" name value)
        (write-header-line name value stream)))
    (dolist (cookie (cookies response))
      (ucw.backend.dribble "Writing cookie header line for ~S" cookie)
      (write-header-line "Set-Cookie"
                         (if (rfc2109:cookie-p cookie)
                             (rfc2109:cookie-string-from-cookie-struct cookie)
                             cookie)
                         stream))
    (write-crlf stream)
    response))

(defmethod send-response ((response basic-response))
  (assert (response-managed-p response))
  (awhen (html-stream response)
    (ucw.backend.dribble "Converting html stream of ~A" response)
    (setf (content response)
          (string-to-octets (get-output-stream-string it) (encoding response)))
    (unless (get-header response "Content-Length")
      (ucw.backend.dribble "Setting Content-Length header of ~A" response)
      (setf (get-header response "Content-Length")
            (princ-to-string (length (content response))))))
  (unless (headers-are-sent-p response)
    (send-headers response))

  (when (and (content response)
             (not (string= "HEAD" (http-method (request response)))))
    (ucw.backend.dribble "Sending ~S (~D bytes) as body"
                         (content response) (length (content response)))
    (write-sequence (content response) (network-stream response))))

;; Copyright (c) 2005-2006 Edward Marco Baringer
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
