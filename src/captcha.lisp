;;; -*- lisp -*-

(in-package :it.bese.ucw)

;; TODO noone drops any captchas, set up some timeout or something so new ones can be generated

(defconstant +captcha-id-length+ 8 "Length of the generated id (which goes into the urls)")

(defvar *number-of-different-captchas-to-keep* 30)
(defvar *captcha-application-url-prefix* "/captcha/")

(defclass captcha-entry-queue (lru-queue)
  ((ids :accessor ids-of
        :initform (make-hash-table :test #'equal))))

(defmethod dequeue :around ((queue captcha-entry-queue) &optional default-value)
  (declare (ignore default-value))
  (let ((entry (call-next-method)))
    (when entry
      (remhash (id-of entry) (ids-of queue)))
    entry))

(defmethod enqueue :after ((queue captcha-entry-queue) entry)
  (setf (gethash (id-of entry) (ids-of queue)) entry))

(defclass captcha-application (minimal-application)
  ((captchas
    :initform (make-instance 'captcha-entry-queue :size *number-of-different-captchas-to-keep*)
    :accessor captchas-of)
   (captcha-factory
    :type (function ())
    :initarg :captcha-factory
    :accessor captcha-factory-of)
   (captcha-length
    :initform 6
    :type (integer 0 100)
    :initarg :captcha-length
    :accessor captcha-length-of))
  (:default-initargs :url-prefix *captcha-application-url-prefix*))

(defparameter *captcha-application* (make-instance 'captcha-application))

(defclass captcha-entry ()
  ((text
    :accessor text-of
    :initarg :text)
   (id
    :accessor id-of
    :initarg :id)
   (url
    :accessor url-of
    :initarg :url)
   (image-data
    :accessor image-data-of
    :initarg :image-data)
   (image-mimetype
    :accessor image-mimetype-of
    :initarg :image-mimetype)))

(defun make-captcha-entry (text image-data &optional image-mimetype)
  (make-instance 'captcha-entry
                 :text text
                 :image-data image-data
                 :image-mimetype (or image-mimetype "image/png")))

(defun drop-all-captchas ()
  (let ((queue (captchas-of *captcha-application*)))
    (iter (repeat (queue-count queue))
          (dequeue queue))))

(defun register-captcha (text image-data &optional image-mimetype)
  (declare (type string text)
           (type (vector (unsigned-byte 8)) image-data))
  (let ((captcha-entry (make-captcha-entry text image-data image-mimetype)))
    (setf (id-of captcha-entry) (new-random-key (ids-of (captchas-of *captcha-application*))
                                                +captcha-id-length+))
    (setf (url-of captcha-entry)
          (strcat (application.url-prefix *captcha-application*)
                  "?id="
                  (id-of captcha-entry)))
    (enqueue (captchas-of *captcha-application*) captcha-entry)
    captcha-entry))

(defun get-captcha ()
  (let ((captcha-count (queue-count (captchas-of *captcha-application*))))
    (when (< captcha-count *number-of-different-captchas-to-keep*)
      ;; fill up the captcha queue if we have less then expected (e.g. startup)
      (iter (repeat (- *number-of-different-captchas-to-keep* captcha-count))
            (multiple-value-bind (image-data image-mimetype text)
                (funcall (captcha-factory-of *captcha-application*) *captcha-application*)
              (register-captcha text image-data image-mimetype)))))
  (let ((captcha-entry (random-queue-element (captchas-of *captcha-application*))))
    (values (text-of captcha-entry) (url-of captcha-entry))))

(defun find-captcha-entry-by-id (id)
  (when id
    (gethash id (ids-of (captchas-of *captcha-application*)))))

(defmethod service ((app captcha-application) context)
  (let* ((id (get-parameter *request* "id"))
         (captcha-entry (find-captcha-entry-by-id id)))
    (setf (get-header *response* "Date") (date:universal-time-to-http-date
                                          (get-universal-time)))
    (if captcha-entry
        (serve-sequence (image-data-of captcha-entry) :content-type "image/png")
        (send-standard-error-page :http-status-code +http-not-found+
                                  :title "The captcha you requested was not found"
                                  :message (format nil "Captcha with id ~S was not found" id)))
    t))

;; Copyright (c) 2003-2005 Attila Lendvai
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
