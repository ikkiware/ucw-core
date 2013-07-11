;;;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defclass standard-component (component)
  ((continuation :accessor component.continuation
                 :initarg :continuation
                 :documentation "Our continuation (a 1 arg lambda)")
   (calling-component :accessor component.calling-component
                      :initarg :calling-component
                      :documentation
"The component which transfered control to this component.

When this component ANSWERs it returns control to the
calling-component and modifes the associated place. Only in the
case of top level components will this slot be NIL.")
   (place :initarg :place
          :accessor component.place
          :documentation
"The place (a PLACE object) which holds this component.

Only in the case of top level component will this slot be NIL.")   
   (dirtyp :initform nil :reader dirtyp
           :documentation "A flag whether the component was invalidated (which means that it needs rendering).")
   (session :initform (context.session *context*)
            :reader session-of
            :documentation "The owner session of this component. A component may only be used in one session at a time.")
   (parent :initarg :parent :accessor parent :initform nil))
  (:documentation "Super class of all standard components."))

(defun initialize-backtracking (comp frame)
  "Prepare the slots of COMP for backtracking."
  (dolist (slot (component-class.backtrack-slots (class-of comp)))
    (backtrack-slot frame comp (c2mop:slot-definition-name slot)
                    (component-slot.backtrack slot))))

(defun initialize-component-slots (comp)
  "Setup any nested components (create them and their places) in COMP.
For every component-slot in COMP: if the slot doesn't already contain
a component and a type was given for what the component should be,
then make-instance of the type and set the slot-value.

Then check whether the slot is bound and not null and if
so set its place slot. the second check is done seperatly from the
first so that components passed via initargs will have their place
set."
  (iter (with class = (class-of comp))
        (for slot :in (component-class.component-slots class))
        (let ((slot-value (when (c2mop:slot-boundp-using-class class comp slot)
                            (c2mop:slot-value-using-class class comp slot))))
          (when (and slot-value
                     (typep slot-value 'standard-component))
            ;; if the slot has already been initialized (due to regular
            ;; initargs) then simply set its place
            (rebind (slot)
              (setf (component.place slot-value)
                    (make-place (slot-value comp (c2mop:slot-definition-name slot)))))
            (setf (parent slot-value) comp)))))

;; we don't use :before and :after because later we may want to hide this primary method
;; when integrating it with different MOP's.
(defmethod (setf c2mop:slot-value-using-class) (new-value
                                               (class standard-component-class)
                                               (instance standard-component)
                                               (slot-def standard-component-effective-slot))
  (let ((component-slot-p (component-slot.component slot-def)))
    (when (and component-slot-p
               (c2mop:slot-boundp-using-class class instance slot-def))
      (awhen (c2mop:slot-value-using-class class instance slot-def)
        (when (typep it 'standard-component)
          (setf (parent it) nil))))
  (prog1
      (call-next-method)
    (when (and component-slot-p
               new-value
               (typep new-value 'standard-component))
      (setf (component.place new-value)
            (make-place (c2mop:slot-value-using-class class instance slot-def)))
      (setf (parent new-value) instance)))))

(defun initialize-place-slot (comp)
  "Setup the place of COMP's place-slot's component.

NB: This function assumes a component already exists in COMP's place-slot."
  (with-slots (place-slot) (class-of comp)
    (when place-slot
      (setf (component.place comp) (make-place (slot-value comp place-slot))))))

(defmethod shared-initialize :after ((c standard-component) slot-names
                                     &key (frame (context.current-frame *context*))
                                     &allow-other-keys)
  "Perform the standard initialization for C.

This method registers C's transaction slot for backtracking and
any other slot which has been declared as backtracked. It then
proceeds to initialize any component slots in C."
  (declare (ignore slot-names))
  ;; setup the 'special' slots for backtracking
  (backtrack-slot frame c 'parent)
  (backtrack-slot frame c 'calling-component)
  ;; do the rest of the initialization
  (initialize-backtracking c frame)
  (initialize-component-slots c)
  (initialize-place-slot c))

(defgeneric child-components (standard-component &key predicate key)
  (:documentation "Return the children of a component."))

(defmethod child-components ((component standard-component) &key predicate (key #'identity))
  "Find all children components of the given components.
 This will only return children components that were declared on the class
as a component and have components in those slots."
  (iter (with class = (class-of component))
        (for slot :in (component-class.component-slots class))
        (when (c2mop:slot-boundp-using-class class component slot)
          (let ((value (c2mop:slot-value-using-class class component slot)))
            (when (and (typep value 'standard-component)
                       (or (null predicate)
                           (funcall predicate (funcall key value))))
              (collect value))))))

(defgeneric descendant-p (parent child &optional recursive-p)
  (:documentation "Predicate to use whether child is a child of parent."))

(defmethod descendant-p ((parent standard-component) (child standard-component) &optional recursive-p)
  (or (eq (parent child) parent)
      (when (and recursive-p (parent child))
        (descendant-p parent (parent child)))))

(defun find-parent-typed (component type &key (otherwise :error))
  (iter (for parent :first (parent component) :then (parent parent))
        (while parent)
        (when (typep parent type)
          (return-from find-parent-typed parent))
        (finally (return-from find-parent-typed
                   (cond ((functionp otherwise)
                          (funcall otherwise))
                         ((eq otherwise :error)
                          (error "Couldn't find component typed ~A among the parents of ~A" type component))
                         ((eq otherwise :warn)
                          (warn "Couldn't find component typed ~A among the parents of ~A" type component)
                          nil)
                         (t otherwise))))))

(defmethod render ((component t))
  (error "No RENDER method defined for ~S." component))

(defvar *copy-down-component* nil
  "Holder variable used for the copy-down link in the inspector
  links. not thread safe (but good enough for single user
  debugging).")

(defun render-with-inspector-link (component call-next-method)
  (<:div :style "border: 2px solid #000000;"
    (<:div :style "margin: 0px; padding: 1px; background: #ccccff; float: top right;"
           (<:a :href (action-href-body (:component component)
                        (call-inspector component component))
           (<:tt "Inspect " (<:as-html component)))
      (when (swank::default-connection)
        (<:as-html " - ")
        (<:a :href (rebind (component)
                     (action-href-body ()
                       (setf *copy-down-component* component)
                       (swank::with-connection ((swank::default-connection))
                         (swank::eval-in-emacs
                          `(slime-repl-send-string "ucw::*copy-down-component*")))
                       (setf *copy-down-component* nil)))
             (<:tt "Copy to slime repl."))))
    (funcall call-next-method)))

(defgeneric (setf dirtyp) (value component))

(defmethod (setf dirtyp) (value (component standard-component))
  (let ((old-value (dirtyp component)))
    (when (xor value old-value)
      (ucw.rerl.ajax.debug "(setf dirtyp) of standard-component switching dirtyp of ~S to ~S" component value)
      (setf (slot-value component 'dirtyp) value)
      (if value
          (register-dirty-component component)
          (unregister-dirty-component component)))))

(defmethod visiblep ((component component))
  (ucw.rerl.ajax.dribble "Checking visiblep of ~S, window is ~S" component (context.window-component *context*))
  (iter (with window = (context.window-component *context*))
        (for parent :first component :then (parent parent))
        (for distance upfrom 0)
        (ucw.rerl.ajax.dribble "At distance ~S and parent ~S" distance parent)
        (when (null parent)
          (return nil))
        (when (eq parent window)
          (return (values t distance)))))

(defmethod render :wrap-around ((component standard-component))
  (ucw.component.render.dribble "Running RENDER of ~A" component)
  (let ((*current-component* component)
        (session (context.session *context*)))
    (aif (session-of component)
         (assert (eq it session))
         (setf (session-of component) session))
    (call-next-method)
    (setf (dirtyp component) nil)))

(defmethod render :wrapping ((component standard-component))
  "Setup up a convience restart, bind *yaclml-stream* and add inspector links."
  (let ((response (context.response *context*)))
    (restart-case
        (if *inspect-components*
            (render-with-inspector-link component #'call-next-method)
            (call-next-method))
      (retry ()
        :report (lambda (stream)
                  (format stream "Retry rendering the component ~S." component))
        (clear-response response)
        (return-from render (render component))))))

(defmethod compute-url ((action action) (component standard-component))
  "Walks up the component tree based at COMPONENT and calls
UPDATE-URL on the components."
  (let ((url (compute-url action (context.application *context*))))
    (labels ((%compute (component)
               ;; first let the parent update the url
               (when (parent component)
                 (%compute (parent component)))
               ;; now that all of our parents have done whatever they
               ;; want with url we can update it.
               (setf url (update-url component url))))
      (%compute component))))

(defmethod update-url ((comp standard-component) url)
  "Do nothing to the URL."
  url)

(defmacro defcomponent (name supers slots &rest options)
  "Macro for defining a component class.

This macro is used to create component classes and provides
options for easily creating the methods which often accompany a
component definition.

NAME, SUPERS and SLOTS as treated as per defclass. The following
extra options are allowed:

 (:ENTRY-POINT url (&key application class)) - Define an
 entry-point on the url URL which simply calls an instance of
 this component. Any request parameters passed to the entry-point
 are used to initialize the slots in the component. This option
 may appear multiple times.

 (:DEFAULT-BACKTRACK function-designator) - Unless the slots
 already have a :backtrack option FUNCTION-DESIGNATOR is
 added. As with the 'regular' :backtrack options if you pass T
 here it is assumed to mean #'IDENTITY.

 (:RENDER (&optional COMPONENT) &body BODY) - Generate a render
 method specialized to COMPONENT. COMPONENT may be a symbol, in
 which case the method will be specialized on the componnet
 class. If COMPONNET is omited the component is bound to a
 variable with the same name as the class.

 (:ACTION &optional NAME) - Generate a defmethod/cc form named
 NAME (which defaults to the name of the component) which simply
 CALL's this component class passing all the arguments passed to
 the action as initargs."
  (macrolet ((destructure-option (option-keyword binding-list &body body)
               `(aif (assoc ,option-keyword options)
                     (destructuring-bind ,binding-list (cdr it)
                       ,@body))))
    (labels ((entry-point-option ()
               (destructure-option :entry-point
                   (url (&key application class))
                 `(defentry-point ,url (,@(if application
                                              `(:application ,application)
                                              '())
                                        ,@(if class
                                              `(:class ,class)
                                              '()))
                      ()
                    (call ',name))))

             (metaclass-option ()
               (or (destructure-option :metaclass
                                       (class-name)
                                       class-name)
                   'standard-component-class))

             (default-backtrack-option ()
               (destructure-option :default-backtrack
                   (copyer)
                 (case copyer
                   (nil nil)
                   ((t) `(:backtrack #'identity))
                   (t `(:backtrack ,copyer)))))

             (action-option ()
               (destructure-option :action
                   (&optional (action-name name))
                 `(defmethod/cc ,action-name ((self ,name) &rest initargs)
                    (apply #'call-component self ',name initargs))))

             (render-option ()
               (destructure-option :render
                   ((&optional (component name))
                    &body body)
                 (unless (listp component)
                   (setf component `(,component ,name)))
                 `(defmethod render (,component)
                    ,@body)))
         
             (effective-options ()
               (remove-if (lambda (option)
                            (member (car option) '(:entry-point :metaclass :default-backtrack
                                                   :action :render)))
                          options))

             (process-slot (slot)
               (destructuring-bind (slot-name &rest initargs)
                   (ensure-list slot)
		 (append
		  (list slot-name)
		  (and (eq 'unspecified
			   (getf initargs :backtrack 'unspecified))
		       (default-backtrack-option))
		  initargs))))
    
      `(progn
         (defclass ,name ,supers
           ,(mapcar #'process-slot slots)
           ,@(effective-options)
           (:metaclass ,(metaclass-option)))
         ,(entry-point-option)
         ,(action-option)
         ,(render-option)
         ',name))))

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
