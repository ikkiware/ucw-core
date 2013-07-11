;; -*- lisp -*-

(in-package :it.bese.ucw.core) 

;;;; ** The mod_lisp backend

(defclass mod-lisp-backend (httpd-backend)
  ()
  (:default-initargs :port 3001))

(defclass mod-lisp-worker (httpd-worker)
  ())
  
(defclass mod-lisp-request (httpd-request)
  ((remote-address :initform nil :initarg :remote-address :accessor remote-address)))

(defclass mod-lisp-response (httpd-response)
  ())

;;;; The mod-lisp-request class methods. Most of the methods are
;;;; actually those defined by the httpd-backend, here we just replace
;;;; the header handling functions.

(defmethod make-response ((request mod-lisp-request))
  (make-instance 'mod-lisp-response
                 :socket (socket request)
                 :request request))

(defmethod read-request ((backend mod-lisp-backend) apache-stream-socket)
  "Read the request (in mod-lisp's format) from the server's
  apache stream. Returns a new request object. Creates a fresh
  stream for each request."
  (let* ((request (make-instance 'mod-lisp-request :socket apache-stream-socket))
         (apache-stream (network-stream request)))
    (ucw.backend.dribble "Reading mod-lisp request from apache socket ~S" apache-stream)
    (iterate
      (for key = (octets-to-string (read-line-from-network apache-stream) :us-ascii))
      (until (string= "end" key))
      (for value = (octets-to-string (read-line-from-network apache-stream)
                                     (if (string= key "url")
                                         #.(or (external-format-for :url) :iso-8859-1)
                                         :iso-8859-1)))
      (when (string= key "remote-ip-addr")
        (setf (remote-address request)
	      (iolib.sockets:string-address-to-vector value)))
      (when (string= key "url")
        (setf (raw-uri request) value)
        (aif (position #\? value)
             (setf (query-path request) (make-displaced-array (raw-uri request) 0 it)
                   (parameters request) (parse-query-parameters
                                         (make-displaced-array (raw-uri request)
                                                               (1+ it))))
             (setf (query-path request) value
                   (parameters request) '()))
        (setf (query-path request) (unescape-as-uri (query-path request))))
      (ucw.backend.dribble "~S=~S" key value)
      (setf (get-header request key) value))
    (setf (parameters request) (append (parameters request)
                                       (accumulate-parameters
                                        (parse-request-body apache-stream
                                                            (get-header request "Content-Length")
                                                            (get-header request "Content-Type")))))
    request))

(defmethod publish-directory ((backend mod-lisp-backend) directory-pathname url-base)
  (ucw.backend.warn
   "Attempting to publish ~S at ~S but mod_lisp backend does not support publish-directory."
   directory-pathname url-base))

;;;; mod-lisp-response

(defun mod-lisp-send-headers (response &optional calculate-content-length-from-body)
  (ucw.backend.dribble "Sending headers for ~S (Status: ~S)." response (status response))
  (let ((network-stream (network-stream response))
        (html-stream (html-stream response)))
    (flet ((out (str)
             (mod-lisp-write-line str network-stream)))
      (out "Status")
      (out (status response))
      (iter (for (key . value) :in (headers response))
            (if (consp value)
                (dolist (v value)
                  (out key)
                  (out v))
                (progn
                  (out key)
                  (out value))))
      (dolist (cookie (cookies response))
	(ucw.backend.dribble "Writing cookie header line for ~S" cookie)
	(out "Set-Cookie")
	(out (if (rfc2109:cookie-p cookie)
		 (rfc2109:cookie-string-from-cookie-struct cookie)
		 cookie)))
      (when calculate-content-length-from-body
        (setf (content response)
              (string-to-octets (get-output-stream-string html-stream)
                                (encoding response)))
        (out "Content-Length")
        (out (format nil "~D" (length (content response)))))
      (out "end"))))

(defmethod send-headers ((response mod-lisp-response))
  (mod-lisp-send-headers response nil))

(defmethod send-response ((response mod-lisp-response))
  (ucw.backend.dribble "Sending mod-lisp response.")
  (mod-lisp-send-headers response t)
  (ucw.backend.dribble "Sending ----")
  (ucw.backend.dribble (content response))
  (ucw.backend.dribble "Done ----")
  (write-sequence (content response) (network-stream response)))

;;;; Helper functions

(defun mod-lisp-write-line (line stream)
  (write-sequence (string-to-octets line :us-ascii) stream)
  (write-byte #.(char-code #\Newline) stream))

;; Copyright (c) 2003-2006 Edward Marco Baringer
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
