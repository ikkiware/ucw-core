;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; This file contains MATCHER API, HANDLER API which both makes up
;;;; the ucw DISPATCHER API.

;;;; * MATCHER API - Matcher classes match the request to return
;;;; appropriate values to handlers so that dispatchers can work
;;;; accordingly.



(defconstant +action-dispatcher-default-priority+       (- most-positive-fixnum 1003))
(defconstant +url-dispatcher-default-priority+          1)

(defclass matcher ()
  ()
  (:documentation "Abstract matcher class."))

(defgeneric matcher-match (matcher application context)
  (:documentation "Abstract method for subclasses to implement a
matcher.  This method would return multiple-values according to
matcher internal nature, but the first return value is always
used to indicate if this matcher matched.

No methods defined on this function may rebind *context*, nor
change CONTEXT's application. Only if the method matches the
request, it is allowed to modify CONTEXT or APPLICATION, even in
that case methods defined on this function must not modify
CONTEXT's application nor rebind *context*."))


;;;; * UCW Request Dispatcher

;;;; Whenever a requset comes into a ucw app we look at its list of
;;;; dispatcher and try to find one which should handle the request.

;;;; NB: Dispatcher instances are a shared resource, the dispatch
;;;; function may be called on the same dispatcher object by multiple
;;;; threads. Take this into consideration when writing custom
;;;; dispatcher classes. IOW: don't put any per-request state in the
;;;; dispatcher instance.

(defclass dispatcher ()
  ((priority :accessor priority
             :initarg :priority
             :initform 0
             :documentation "Dispatchers will be checked from
             highest-priority to lowest priority. The default
             values for priority on the various classes assume
             this is a positive integer less than
             most-positive-fixnum.")))

(defgeneric dispatch (dispatcher application context)
  (:documentation "Entry point into a dispatcher. Must return T
  if the context has been handled or NIL if it hasn't.

No methods defined on this function may rebind *context*, nor
change CONTEXT's application. Only if the method returns T is it
allowed to modify CONTEXT or APPLICATION, even in that case
methods defined on this function must not modify CONTEXT's
application nor rebind *context*."))

(defmethod dispatch ((dispatcher dispatcher)
                     (application basic-application)
                     (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "In dispatch, now calling matcher-match for (~S ~S ~S)" dispatcher application context)
  (let ((result (multiple-value-list (matcher-match dispatcher application context))))
    (ucw.rerl.dispatcher.dribble "Result is ~S" result)
    (when (and (consp result)
               (car result))
      (ucw.rerl.dispatcher.debug "~S matched, calling handler-handle with (~S ~S ~S)" dispatcher application context (rest result))
      (handler-handle dispatcher application context (rest result))
      t)))

(defclass session-frame-matcher (matcher)
  ((frame-is-optional-p :accessor frame-is-optional-p :initarg :frame-is-optional-p))
  (:default-initargs :frame-is-optional-p nil)
  (:documentation "Matches when a valid session and a valid frame
could be identified from the request."))

(defmethod matcher-match ((matcher session-frame-matcher)
                          (application basic-application)
                          (context standard-request-context))
  
  (ucw.rerl.dispatcher.dribble "~S trying to match as session-frame-matcher, session-id is ~S, frame-id is ~S"
                               matcher (find-session-id context) (find-frame-id context))
  (when-bind session (find-session application context)
    (ucw.rerl.dispatcher.dribble "~S matched session ~S" matcher session)
    (let ((frame-id (find-frame-id context)))
      (if-bind frame (find-frame-by-id session frame-id)
        (progn
          (ucw.rerl.dispatcher.dribble "~S matched frame ~S" matcher frame)
          (values t session frame))
        (progn
          (ucw.rerl.dispatcher.dribble "~S NOT matched any frame" matcher)
          (values (frame-is-optional-p matcher) session nil))))))

(defclass url-matcher (matcher)
  ((url-string :initform nil
               :initarg :url-string
               :accessor url-string))
  (:documentation "Matcher used to match url-string exactly (using string=)."))

(defprint-object (self url-matcher)
  (princ (url-string self)))

(defmethod matcher-match ((matcher url-matcher)
                          (application basic-application)
                          (context standard-request-context))
  "Returns matched url-string to handler."
  (ucw.rerl.dispatcher.dribble "~S trying to match as url-matcher, ~S = ~S"
                               matcher (query-path-sans-prefix context) (url-string matcher))
  (when (string= (url-string matcher) (query-path-sans-prefix context))
    (ucw.rerl.dispatcher.dribble "~S matched" matcher)
    (values t (url-string matcher))))

;;;; * HANDLER API - Handler classes are used to handle requests that
;;;; * are matched by matchers. Matcher results are passed as last
;;;; * parameter for handlers use.

(defclass handler ()
  ()
  (:documentation "Abstract handler class."))

(defgeneric handler-handle (handler application context matcher-result)
  (:documentation "Abstract function for handler classes to
implement the handling of the matched request.

These methods may modify context as they wish since they are
matched, request will be closed after this method is run."))

(defclass entry-point-handler ()
  ((handler :accessor handler
            :initarg :handler
            :documentation "Function to run when this
           entry-point-handler runs. This handler is a
           zero-arged."))
  (:documentation "This handler is used to handle
  entry-points."))

(defmethod initialize-instance :after ((handler entry-point-handler) &key) nil
  #+nil(unless (typep (handler handler) 'action)
	 (setf (handler handler) (make-action (handler handler)
                                         :class 'basic-action))))

(defmethod handler-handle ((handler entry-point-handler)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (ucw.rerl.dispatcher.debug "~S is handling the request" handler)
  (let* ((session (ensure-session application context)))
    (handle-action (handler handler) application session
                   (session.current-frame session))))



;;;; ** URL-DISPATCHER

(defclass url-dispatcher (dispatcher url-matcher entry-point-handler)
  ()
  (:default-initargs :priority +url-dispatcher-default-priority+))

(defmacro make-url-dispatcher (url-string &body body)
  "Returns a dispatcher which matches when URL-STRING matches and
executes BODY in a with-call/cc block."
  `(make-instance 'url-dispatcher
                  :url-string ,url-string
                  :handler (lambda ()
                             (with-call/cc
                               (let ((self nil))
                                 ,@body)))))

;;;; ** ACTION-DISPATCHER

(defclass action-dispatcher (dispatcher session-frame-matcher)
  ()
  (:default-initargs :priority +action-dispatcher-default-priority+)
  (:documentation "This is the core dispatcher for ucw. Due to
  how ucw's COMPUTE-URL method works it is important that the
  action-dispatcher be checked before any url-dispatchers, so it
  should be the first element in an application's dispatcher
  list."))

;; TODO there's a race condition: between matching the action and handling it (probably the
;; application should be kept locked up til the point when action is ready to be called. then lock
;; the session and release the app.)
(defmethod matcher-match ((matcher action-dispatcher)
                          (application basic-application)
                          (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as action-dispatcher action-id is ~S"
                               matcher (find-action-id context))
  (multiple-value-bind (matchesp session frame) (call-next-method)
    (when matchesp
      (multiple-value-bind (action-request-p action-id)
          (action-request-p context)
        (when action-request-p
          (let ((action (find-action frame action-id)))
            (setf (context.action *context*) action)
            (values t session frame action)))))))

(defmethod handler-handle ((dispatcher action-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (destructuring-bind (session frame action) matcher-result
    (unless action
      (error 'action-not-found))
    (ensure-session application context session)
    (handle-action action application session frame)))



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
