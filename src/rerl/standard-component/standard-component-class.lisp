;;;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defclass standard-component-slot ()
  ((backtrack :initform nil :initarg :backtrack :accessor component-slot.backtrack)
   (component :initform nil :initarg :component :accessor component-slot.component))
  (:documentation "The superclass for direct/effective slots of standard-components.

Other than the initargs for standard slots the following
options can be passed to component slots:

:backtrack [ T | NIL | FUNCTION-NAME ] - Specify that this slot
should be backtracked (or not if NIL is passed as the value). If
the value is neither T nor NIL then it must be a function which
will be used as the copyer.

:component [ T | NIL | TYPE | ( TYPE &rest INITARGS ) ] - Specify that this
slot is actually a nested component of type TYPE unless NIL. When instances
of the class are created and there's no iniform for this slot, then this it
will be set to an instance of type TYPE and its place will be set to this
slot. If a list is passed to :component then TYPE (which isn't evaluated) will be
passed as the first argument to make-instance. The INITARGS will
be eval'd and apply'd to make-instance. The result of this call
to make-instance will be used as the component object used for initialization."))


;;;; STANDARD-COMPONENT-CLASS meta object

(defclass standard-component-class (standard-class)
  ((backtrack-slots :initform nil :accessor component-class.backtrack-slots
                    :documentation "A list of effective-slot-definition's that are backtracked.")
   (component-slots :initform nil :accessor component-class.component-slots
                    :documentation "A list of effective-slot-definition's that are component slots.")
   (place-slot :initform nil :initarg :place-slot
               :accessor component-class.place-slot))
  (:documentation "The meta class for standard-components.

Classes defined with this meta-class have extra slot options, see
the class STANDARD-COMPONENT-DIRECT-SLOT for details."))




(defmethod c2mop:validate-superclass ((component-class standard-component-class)
                                     (super-class standard-class))
  "Declare that standard-component-classes can be sub classes of standard-class."
  t)

(defmethod c2mop:finalize-inheritance :after ((class standard-component-class))
  ":after initialization function for standard-component-class objects.

Setup the proper values in component-class.backtrack-slots and
component-class.component-slots in CLASS based on the effective
slots of CLASS."
  (iter (for slot in (c2mop:class-slots class))
        (when (typep slot 'standard-component-slot)
          (when (component-slot.backtrack slot)
            (collect slot :into backtracked-slots))
          (when (component-slot.component slot)
            (collect slot :into component-slots)))
        (finally
         (setf (component-class.backtrack-slots class) backtracked-slots
               (component-class.component-slots class) component-slots)))
  (setf (component-class.place-slot class) (first (component-class.place-slot class))))


(defun fixup-component-slots (direct-slots)
  (mapcar
    (lambda (slot)
      (append
       (if-bind component-spec (and (eq 'unspecified
					(getf slot :initform 'unspecified))
				    (getf slot :component))
		(if (not (eq component-spec t))
		    (list
		     :initform
		     (etypecase component-spec
		       (cons `(make-instance ',(first component-spec) ,@(rest component-spec)))
		       (symbol `(make-instance ',component-spec)))
		     :initfunction
		     (etypecase component-spec
		       (cons
			;; compile a lambda expression containing the
			;; expansion of the :initform -- otherwise
			;; things like :component (foo :bar `((baz
			;; . ,quux))) won't work.
			(compile nil
				 `(lambda ()
				    (make-instance ',(first component-spec)
						   ,@(rest component-spec)))))
		       (symbol (lambda ()
				 (make-instance component-spec)))))))
       slot))
    direct-slots))



;;; Ensure standard-component is among the supers of the instances of standard-component-class.
;;; Thanks to Pascal Constanza for the code.
(defmethod initialize-instance :around ((class standard-component-class) &rest initargs
                                        &key direct-superclasses direct-slots)
  (declare (dynamic-extent initargs))
  (let ((newargs
	 (append
	  (unless (loop for class in direct-superclasses
		     thereis (ignore-errors
			       (subtypep class (find-class 'standard-component))))
	    (list
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'standard-component)))))
	  (list :direct-slots (fixup-component-slots direct-slots))
	  initargs)))
    (apply #'call-next-method class newargs)))

(defmethod reinitialize-instance :around ((class standard-component-class) &rest initargs
                                          &key (direct-superclasses '() direct-superclasses-p) (direct-slots '() direct-slots-p))
  (declare (dynamic-extent initargs))
  (let ((newargs
	 (append
	  ;; if direct superclasses are explicitly passed
	  ;; this is exactly as in INITIALIZE-INSTANCE
	  ;; if direct superclasses are not explicitly passed
	  ;; we _must_ not change anything
	  (unless (or (not direct-superclasses-p)
		       (loop for class in direct-superclasses
			  thereis (ignore-errors
				    (subtypep class (find-class 'standard-component)))))
	    (list
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'standard-component)))))
	  (when direct-slots-p
	    (list :direct-slots (fixup-component-slots direct-slots)))
	  initargs)))
    (apply #'call-next-method class newargs)))


;;; Component slot metaobjects



(defclass standard-component-direct-slot (standard-component-slot c2mop:standard-direct-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class standard-component-class)
                                              &rest slot-initargs)
  (if (needs-to-be-standard-component-direct-slot-p slot-initargs)
      (find-class 'standard-component-direct-slot)
      (call-next-method)))

(defun needs-to-be-standard-component-direct-slot-p (slot-initargs)
  (iter (for name :in slot-initargs :by #'cddr)
        (when (member name '(:backtrack :component))
          (return-from needs-to-be-standard-component-direct-slot-p t)))
  nil)

(defclass standard-component-effective-slot (standard-component-slot c2mop:standard-effective-slot-definition)
  ())

(defun first-specifying-slot (direct-slot-definitions slot-reader)
  (dolist (slot direct-slot-definitions)
    (when (typep slot 'standard-component-slot)
      (etypecase slot-reader
        (function (when (funcall slot-reader slot)
                    (return-from first-specifying-slot slot)))
        (symbol (when (slot-boundp slot slot-reader)
                  (return-from first-specifying-slot slot))))))
  nil)

(defun needs-to-be-standard-component-effective-slot-p (direct-slot-definitions)
  (iter (for direct-slot-definition :in direct-slot-definitions)
        (when (typep direct-slot-definition 'standard-component-direct-slot)
          (return-from needs-to-be-standard-component-effective-slot-p
            (or (component-slot.component direct-slot-definition)
                (component-slot.backtrack direct-slot-definition)))))
  nil)

(defmethod c2mop:effective-slot-definition-class ((class standard-component-class)
                                                 &key &allow-other-keys)
  (declare (special %effective-slot-definition-class%))
  (aif %effective-slot-definition-class%
       (find-class it)
       (call-next-method)))

(defmethod c2mop:compute-effective-slot-definition :around ((class standard-component-class)
                                                           slot-name
                                                           direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((%effective-slot-definition-class%
         (when (needs-to-be-standard-component-effective-slot-p
                direct-slot-definitions)
          'standard-component-effective-slot)))
    (declare (special %effective-slot-definition-class%))
    (let ((effective-slot (call-next-method)))
      (when (typep effective-slot 'standard-component-effective-slot)
        (flet ((effective-backtrack-value (backtrack-spec)
                 (if (eql t backtrack-spec)
                     #'identity
                     (eval backtrack-spec)))
               (effective-component-value (component-spec)
                 component-spec))
          ;; do we want backtracking for this slot?
          (when-bind slot (first-specifying-slot direct-slot-definitions 'backtrack)
            (setf (component-slot.backtrack effective-slot)
                  (effective-backtrack-value (slot-value slot 'backtrack))))
          ;; is this slot a component slot?
          (when-bind slot (first-specifying-slot direct-slot-definitions 'component)
            (let ((component-spec (slot-value slot 'component)))
              ;; make sure the slot is bound. this will mean later, when putting
              ;; a component in this effective slot, to update the place slot
              ;; of the component being put. (with a slot-value-using-class override)
              (setf (component-slot.component effective-slot)
                    (effective-component-value component-spec))
              (when component-spec
                ;; component-slots also have backtracking (unless
                ;; explicitly overridden)
                (setf (component-slot.backtrack effective-slot)
                      (if (component-slot.backtrack slot)
                          (effective-backtrack-value (slot-value slot 'backtrack))
                          #'identity)))))))
      effective-slot)))

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
