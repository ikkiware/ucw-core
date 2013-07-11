(in-package :ucw-standard)

(defparameter *source-component* nil)

(defclass standard-action (basic-action)
  ((source-component :accessor action-source-component))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod shared-initialize :before ((action standard-action) slots &rest args)
  (declare (ignore slots args))  
  (setf (action-source-component action) *source-component*))

(defmethod handle-action :around ((action standard-action) a s f)
  (let ((*source-component* (action-source-component action)))
    (call-next-method)))

(defun/cc call (thing &rest args)
  (call-component *source-component*
		  (etypecase thing
		    (symbol (apply #'make-instance thing args))
		    (standard-object thing))))

(defun/cc call-as-window (component-type &rest component-init-args)
  "Just like CALL but the new component is used for the entire
window. In other words the new component will be used to render
the entire browser window, independant of the current component's
position. The new component must therefore remember to render as
an entire html page.

This is useful for dealing with modal component like alerts and
dialogs which must be dealt with before the user can continue."
  (call-component (context.window-component *context*)
		  (apply #'make-instance component-type component-init-args)))

(defun/cc jump (component-type &rest component-init-args)
  (jump-to-component (apply #'make-instance component-type component-init-args)))

(defun/cc answer (&optional val)
  (let ((child *source-component*))
    (setf *source-component* (component.calling-component child))
    (answer-component child val)))

(defmacro defaction (&rest args-and-body)
  `(arnesi:defmethod/cc ,@args-and-body))

(defvar +action-compound-name-delimiter+ #\|)

(defmethod find-action-id :around ((context standard-request-context))
  (or 
   (loop
      :for (k . v) in (parameters 
		      (context.request context))
      :do(destructuring-bind (param-name &optional action-id)
	      (split-sequence:split-sequence 
	       +action-compound-name-delimiter+ k)
	    (when (and action-id 
		       (string= 
			+action-parameter-name+ param-name))
	      (return action-id))))
   (call-next-method)))