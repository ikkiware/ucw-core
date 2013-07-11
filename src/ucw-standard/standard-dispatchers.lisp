;; -*- lisp -*-
(in-package :ucw-standard)

(defvar *dispatcher-registers*)
(defvar *dispatcher-url-suffix*)

(defconstant +minimal-dispatcher-default-priority+      1)
(defconstant +simple-dispatcher-default-priority+       1)
(defconstant +starts-with-dispatcher-default-priority+  1)
(defconstant +regex-dispatcher-default-priority+        1)

(defclass starts-with-matcher (url-matcher)
  ())

(defmethod matcher-match ((matcher starts-with-matcher)
                          (application basic-application)
                          (context standard-request-context))
  "Returns matched url-string to handler."
  (ucw-log:ucw.rerl.dispatcher.dribble "~S trying to match as starts-with-matcher, ~S starts-with? ~S"
				       matcher (query-path-sans-prefix context) (url-string matcher))
  (multiple-value-bind (matchedp suffix)
      (starts-with (query-path-sans-prefix context) (url-string matcher)
		   :return-suffix t)
    (ucw-log:ucw.rerl.dispatcher.dribble "~S matched" matcher)
    (values matchedp suffix)))

(defclass regexp-url-matcher (url-matcher)
  ((scanner :initarg :scanner
            :accessor scanner
            :documentation "CL-PPCRE scanner used for pattern
            matching. Created automagically when url-string is
            set via accessors or initform."))
  (:documentation "Regexp matcher class to match url-string
  via cl-ppcre scanner."))

(defmethod shared-initialize :after ((matcher regexp-url-matcher)
                                     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  ;; trigger our customized accessor below
  (awhen (url-string matcher)
    (setf (url-string matcher) it)))

(defmethod (setf url-string) :after (value (matcher regexp-url-matcher))
  (setf (scanner matcher) (cl-ppcre:create-scanner value)))

(defmethod matcher-match ((matcher regexp-url-matcher)
                          (application basic-application)
                          (context standard-request-context))
  "Returns two values to handler on success: the whole match as a
string plus an array of substrings (or NILs) corresponding to the
matched registers."
  (ucw-log:ucw.rerl.dispatcher.dribble "~S trying to match ~S" matcher (query-path-sans-prefix context))
  (let ((result
         (multiple-value-list
           (cl-ppcre:scan-to-strings (scanner matcher)
                                     (query-path-sans-prefix context)
                                     :sharedp t))))
    (when (car result)
      (ucw-log:ucw.rerl.dispatcher.dribble "~S matched, regexp result is ~S" matcher result)
      (values-list
       (cons t result)))))

(defclass function-handler (handler)
  ((handler :accessor handler
            :initarg :handler
            :documentation "Called when the dispatchers finds a
            maching request. This function must be a zero arg'ed
            (lambda ()
               ...)"))
  (:documentation "Function handler class funcalls the handler
  lambda while providing application and context. "))

(defmethod handler-handle ((function-handler function-handler)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (when (slot-boundp function-handler 'handler)
    (ucw-log:ucw.rerl.dispatcher.debug "~S running handler ~S" function-handler (handler function-handler))
    (funcall (handler function-handler))))

(defclass minimal-dispatcher (dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +minimal-dispatcher-default-priority+)
  (:documentation "A dispatcher which does as little work as
  possible. The handler function must do basically
  everything (including shutdowning down request and response)."))

(defclass simple-dispatcher (dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +simple-dispatcher-default-priority+)
  (:documentation "This class of dispatchers avoids all of UCW's
  standard call/cc (and therefore frame/backtracking/component)
  mechanism.

Unlike all other UCW dispatchers a simple-dispatcher must not use
CALL, and must perform the rendering directly within the handler."))

(defmethod handler-handle ((dispatcher simple-dispatcher)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (ensure-session application context)
  (call-next-method))

(defclass starts-with-dispatcher (dispatcher
				  starts-with-matcher
				  starts-with-binding-handler
				  entry-point-handler)
  ()
  (:default-initargs :priority +starts-with-dispatcher-default-priority+))

(defclass starts-with-binding-handler ()
  ())

(defmethod handler-handle :around ((handler starts-with-binding-handler)
                                   application
                                   context
                                   matcher-result)
  (let ((*dispatcher-url-suffix* (car matcher-result)))
    (call-next-method)))

(defclass regexp-dispatcher (dispatcher
                             regexp-url-matcher
                             regexp-binding-handler
                             entry-point-handler)
  ()
  (:default-initargs :priority +regex-dispatcher-default-priority+)
  (:documentation "Matches URL using a cl-ppcre regular
expression. Captured registers are available via
`ucw:*dispatcher-registers*'"))

(defclass regexp-binding-handler ()
  ())

(defmethod handler-handle :around ((handler regexp-binding-handler)
                                   application
                                   context
                                   matcher-result)
  (let ((*dispatcher-registers* (second matcher-result)))
    (call-next-method)))

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
