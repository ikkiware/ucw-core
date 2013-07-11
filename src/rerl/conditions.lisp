;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(define-condition no-handler-for-request (error)
  ((raw-uri :initarg :raw-uri :accessor raw-uri-of
            :initform "#<unknown>")
   (request :initarg :request :initform nil :accessor request-of))
  (:documentation "This is signalled for a 404")
  (:report (lambda (error stream)
             (format stream "No handler for query: ~S~%" (raw-uri-of error)))))

;; TODO these conditions should be used more

;;;; ** RERL Conditions

(define-condition rerl-error (error)
  ((context :initarg :context :accessor context-of :initform (when (boundp '*context*)
                                                               *context*)))
  (:documentation "An error signalled during the request processing chain."))

(define-condition too-many-sessions (rerl-error)
  ()
  (:documentation "This error is signalled when there are more then
*maximum-number-of-sessions* live sessions and a new one should be needed."))


;;;; Conditions relating to badly formed requests. Generally this means
;;;; that some essential piece of information was either missing or
;;;; unrecognizable.

(define-condition inexistent-request-part (rerl-error)
  ((query-path :initarg :query-path :accessor query-path
               :initform "#<unknown>"))
  (:documentation "Class of errors signaled when a particular
part of the action chain (session, frame, param-action or action)
is specified in the request but the corresponding server side
object can't be found.")
  (:report report-inexistent-request-part))

(defun report-inexistent-request-part (error stream)
  (format stream "Missing request part error for query-path ~S, type ~A~%" (query-path error) (type-of error)))

(defmethod initialize-instance :after ((error inexistent-request-part) &key)
  (when (and (boundp '*request*)
             *request*)
    (setf (query-path error) (query-path *request*))))

(define-condition badly-formatted-request (rerl-error)
  ()
  (:documentation "Class of errors signaled when a particular
  id (application, session, frame or action) is not found in the
  request."))

(define-condition request-content-length-limit-reached (badly-formatted-request)
  ((content-length :initform nil
                   :initarg :content-length
                   :accessor content-length-of)
   (request-content-length-limit :initform *request-content-length-limit*
                                 :initarg :request-content-length-limit
                                 :accessor request-content-length-limit-of))
  (:report (lambda (error stream)
             (format stream "The content-length of the request is too large according to the server policy (~A > ~A), see *REQUEST-CONTENT-LENGTH-LIMIT*"
                     (content-length-of error)
                     (request-content-length-limit-of error)))))

(defun request-content-length-limit-reached (content-length)
  (error 'request-content-length-limit-reached :content-length content-length))

(define-condition session-id-missing (badly-formatted-request)
  ())

(define-condition frame-id-missing (badly-formatted-request)
  ())

(define-condition action-id-missing (badly-formatted-request)
  ())


(define-condition session-is-invalid (inexistent-request-part)
  ((session :accessor session-of
            :initarg :session)))

(define-condition session-has-expired (session-is-invalid)
  ())

(define-condition frame-not-found (inexistent-request-part)
  ())

(define-condition action-not-found (inexistent-request-part)
  ())


(define-condition callback-error (rerl-error)
  ()
  (:documentation "An error has occured while handling a callback."))

(define-condition action-error (rerl-error)
  ()
  (:documentation "An error has occured during the execution of an action."))

(define-condition render-error (rerl-error)
  ()
  (:documentation "An error has occured while rendering a component."))

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
