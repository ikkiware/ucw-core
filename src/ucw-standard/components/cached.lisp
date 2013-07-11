;; -*- lisp -*-

(in-package :ucw-standard)

;;;; ** Caching Component

(defcomponent cache-component-mixin ()
  ((cached-output :accessor component-cache :initform nil
                  :documentation "A string holding the output to use
                  for this component. This string will be written
                  directly to the html stream." ))
  (:documentation "Component which caches its output.
0
The component caching protocol consists of the generic functions
COMPONENT-CACHE-INVALID-P, REFRESH-COMPONENT-OUTPUT, and
NOTE-CACHE-UPDATED. Caching is managed in a :wrapping method on RENDER."))

(defgeneric component-cache-invalid-p (component)
  (:documentation "Returns T if COMPONENT's cache is invalid."))

(defgeneric note-cache-updated (component output)
  (:documentation "Called whenever COMPONENT's cache is updated. It is
  safe to override the primary method.")
  (:method ((component cache-component-mixin) output) t))

(defmethod component-cache-invalid-p :around ((c cache-component-mixin))
  (and (component-cache c) (call-next-method)))
 
(defmethod render :wrapping ((c cache-component-mixin))
  (when (component-cache-invalid-p c)
    (setf (component-cache c)
          (with-output-to-string (yaclml:*yaclml-stream*)
            (call-next-method)))
    (note-cache-updated c (component-cache c)))
  (write-sequence (component-cache c)
		  (html-stream (context.response *context*))))

;;;; ** Timeout cache component

(defcomponent timeout-cache-component-mixin (cache-component-mixin)
  ((last-refresh :accessor last-refresh :initform nil
                 :documentation "The time, exrpessed as a
                 universal time, when the component was last rendered.")
   (expires-after :accessor expires-after :initarg :expires-after
		  :documentation "Number of seconds the cache is valid."))
  (:default-initargs
   :expires-after (* 30 60 60))
  (:documentation "Render the component at most every EXPIRES-AFTER seconds."))

(defmethod component-cache-invalid-p ((c timeout-cache-component-mixin))
  (if (null (last-refresh c))
      ;; hasn't been rendered yet.
      t
      (< (last-refresh c)
         (- (get-universal-time) (expires-after c)))))

(defmethod note-cache-updated ((c timeout-cache-component-mixin) output)
  (declare (ignore output))
  (setf (last-refresh c) (get-universal-time)))

;;;; ** Num hits cache component

(defcomponent num-hits-cache-component-mixin (cache-component-mixin)
  ((hits-since-refresh :accessor hits-since-refresh
                       :initform nil
                       :documentation "Number of views since last refresh.")
   (hits :accessor hits :initarg :hits
	 :documentation "Number of before the cache is invalid."))
  (:default-initargs :hits 10)
  (:documentation "Render the component every HITS views."))

(defmethod component-cache-invalid-p ((n num-hits-cache-component-mixin))
  (if (null (hits-since-refresh n))
      t
      (< (hits n) (hits-since-refresh n))))

(defmethod render :after ((n num-hits-cache-component-mixin))
  (incf (hits-since-refresh n)))

(defmethod note-cache-updated ((n num-hits-cache-component-mixin) output)
  (declare (ignore output))
  (setf (hits-since-refresh n) 0))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; Copyright (c) 2008 Clinton Ebadi
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
