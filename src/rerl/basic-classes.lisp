;;;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defclass minimal-application (application)
  ((url-prefix :accessor application.url-prefix
               :initarg :url-prefix
               :initform ""
               :documentation "A string specifying the
start (prefix) of all the urls this app should handle.

This value is used by the standard-server to decide what app a
particular request is aimed at and for generating links to
actions within the app. ")
   (request-context-class :accessor request-context-class-of
                          :documentation "Caches the class of the effective request context.")
   (server :accessor application.server
           :initform *default-server*
           :initarg :server)
   (lock :accessor lock-of)
   (debug-on-error :initarg :debug-on-error :accessor debug-on-error)))

(defclass basic-application (application-with-session-handling-mixin minimal-application)
  ((charset :accessor application.charset
            :initarg :charset
            :initform #-:sb-unicode nil #+:sb-unicode :utf-8
            :documentation "Default charset for sent text/html documents.")
   (dispatchers :initform nil
                :initarg :dispatchers
                :accessor application.dispatchers
                :documentation "A list of request
dispatchers. The user supplied list of dispatchers is extended
with other dispatchers that are required for UCW to function
properly (action-dispatcher, a parenscript-dispatcher, etc). If
you want full control over the active dispatchers use the (setf
application.dispatchers) accessor or, if you want control over
the order of the dispathcers, (slot-value instance
'dispatchers)."))
  (:default-initargs :dispatchers (list (make-instance 'action-dispatcher)))
  (:documentation "The base UCW application class."))

(defprint-object (app basic-application)
  (write (application.url-prefix app)))

(defmethod initialize-instance :after ((self basic-application) &key)
  (setf (lock-of self) (make-recursive-lock
                        (strcat "Application lock for "
                                (application.url-prefix self)))))

(defclass application-with-session-handling-mixin (application)
  ((session-class :accessor session-class-of
                  :documentation "Caches the class of the effective session.")
   (session-table :accessor application.session-table
                  :initform (make-hash-table :test 'equal)))
  (:documentation "This mixin adds session handling capabilities to applications."))

(defclass standard-request-context (request-context)
  ((request     :accessor context.request     :initarg :request     :initform nil)
   (response    :accessor context.response    :initarg :response    :initform nil)
   (application :accessor context.application :initarg :application :initform nil)
   (session     :accessor context.session     :initarg :session     :initform nil)
   (action      :accessor context.action      :initarg :action      :initform nil)))

(defclass standard-server (server)
  ((applications :accessor server.applications
                 :initform nil)
   (lock :accessor lock-of :initform (make-lock "Server lock"))
   (started :accessor server.started :initform nil :initarg :started)
   (backend :accessor server.backend
            :initform nil
            :initarg :backend)))

(defclass standard-session-frame (session-frame)
  ((actions :accessor frame.actions :initform (make-hash-table :test 'equal)
            :documentation "A hash table mapping action ids to 0 argument functions.")
   (callbacks :accessor frame.callbacks :initform (make-hash-table :test 'equal)
              :documentation "A hash table mapping callback ids to 1 argument functions.")
   (window-component :accessor frame.window-component :initarg :window-component :initform nil
                     :documentation "The root component for this
frame. The standard-server calls render on this component when
the frame is ready to be presented to the user.")
   (id :initarg :id :accessor frame.id :initform nil)
   (allocated-backtracks :initform (make-array 16 :adjustable t :fill-pointer 0)
                         :accessor allocated-backtracks-of
                         :documentation "The places allocated in this frame.")
   (effective-backtracks :initarg :effective-backtracks
                         :initform (make-hash-table :test #'eq)
                         :accessor effective-backtracks-of
                         :documentation "PLACE -> VALUE mapping of the effective backtracked places.")))

(defprint-object (frame standard-session-frame)
  (format *standard-output* "~A ~D/~D/~D"
          (frame.id frame)
          (hash-table-count (frame.actions frame))
          (hash-table-count (frame.callbacks frame))
          (hash-table-count (effective-backtracks-of frame))))


(defclass place ()
  ((getter :accessor place.getter :initarg :getter)
   (setter :accessor place.setter :initarg :setter)
   (copyer :accessor place.copyer :initarg :copyer)
   (form   :accessor place.form   :initarg :form))
  (:documentation "A \"pointer\" or \"locative\", an object
encapsulating a settable and readable place"))

(defprint-object (p place)
  (write (place.form p) :readably nil :circle t))

(defclass basic-session (session)
  ((frames :initform (make-instance 'frame-queue :size +session-backtracking-max-depth+)
           :accessor session.frames
           :documentation "The table of session-frame objects
generated in this session.")
   (current-frame :initform nil
                  :accessor session.current-frame)
   (id :initarg :id :initform nil :accessor session.id)
   (last-access :initarg :last-access
                :accessor session.last-access
                :initform (get-universal-time))
   (object-table :initarg :object-pool
                 :accessor session.object-pool
                 :initform (make-hash-table :test 'eql))
   (live-backtracks
    :initform (make-hash-table :test #'eq)
    :accessor live-backtracks-of
    :documentation "A cumulated hashtable of all the ALLOCATED-BACKTRACKS of each frame.
When a new frame is created, only these backtracks are cloned. When a frame is
dropped, its ALLOCATED-BACKTRACKS are remhash's from here. Because weak pointers
are too expensive for this.")
   (lock :accessor lock-of)
   (session-frame-class :accessor session-frame-class-of
			:documentation "Caches the class of the effective session frame.")))

(defmethod initialize-instance :after ((self basic-session) &key)
  (setf (lock-of self) (make-recursive-lock (strcat "Session lock for "
                                                    (session.id self)))))

(defprint-object (s basic-session)
  (format *standard-output* "~D ~S" (queue-count (session.frames s)) (session.current-frame s)))


(defclass action ()
  ((action-lambda
    :accessor action-lambda-of
    :initarg :lambda
    :initform (lambda ()
                (error "No action function"))))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation ""))

(defclass renderable-action (action)
  ((id
    :initform nil
    :initarg :id
    :type (or null string)
    :accessor action-id))
  (:metaclass c2mop:funcallable-standard-class))

;; TODO maybe these booleans should be in the type of the action
(defclass basic-action (renderable-action)
  ((call-callbacks
    :initform t
    :initarg :call-callbacks
    :type boolean
    :accessor action-call-callbacks-p)
   (call-render
    :initarg :call-render
    :type boolean
    :accessor action-call-render-p)
   (make-new-frame
    :initform t
    :initarg :make-new-frame
    :type boolean
    :accessor action-make-new-frame-p))
  (:metaclass c2mop:funcallable-standard-class)
  (:default-initargs :call-render t))

(defclass action-with-isolation-support (basic-action)
  ((valid
    :initform t
    :type boolean
    :accessor action-valid-p
    :documentation "Invalid actions are never called anymore.")
   (isolated
    :initform nil
    :initarg :isolated
    :type boolean
    :accessor action-isolated-p
    :documentation "Isolated actions are invalidated after the first call.")
   (backtracks :accessor action-backtracks :initform nil))
  (:metaclass c2mop:funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
