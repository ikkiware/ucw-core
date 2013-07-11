;; -*- lisp -*-

(in-package :ucw-standard)

(defclass transactional-application-mixin ()
  ())

(defclass transactional-session-mixin ()
  ((transaction-stack :initarg :transaction-stack
                      :accessor session.transaction-stack
                      :initform nil)))

(defclass transactional-session-frame-mixin ()
  ((transaction-stack :initarg :transaction-stack
                      :accessor frame.transaction-stack
                      :initform nil)))

(defmethod session-class list ((application transactional-application-mixin))
  'transactional-session-mixin)

(defmethod session-frame-class list ((session transactional-session-mixin))
  'transactional-session-frame-mixin)

(defgeneric/cc open-transaction* (session))
(defgeneric/cc close-transaction* (session))

(defun/cc open-transaction (&optional (session (context.session *context*)))
  (open-transaction* session))

(defun/cc close-transaction (&optional (session (context.session *context*)))
  (close-transaction* session))

(defmethod/cc open-transaction* ((s transactional-session-mixin))
  (push :open (session.transaction-stack s))
  (ucw-core::make-new-frame nil s))

(defmethod/cc close-transaction* ((s transactional-session-mixin))
  (let/cc k
    (let ((transaction-stack (session.transaction-stack s)))
      (let (fixed-k)
        (setf fixed-k (lambda (v)
                        (declare (ignore v))
                        (setf (car transaction-stack) (cons fixed-k
							    (context.current-frame *context*))
                              (session.transaction-stack s) (cdr transaction-stack))
			(ucw-core::make-new-frame nil s)
                        (kall k t)))
	(funcall fixed-k :whatever)))))

(defmethod ucw-core::make-new-frame :around
    (action (session transactional-session-mixin))
  (let ((frame (call-next-method)))
    (setf (frame.transaction-stack frame) (session.transaction-stack session))
    frame))

(defmethod call-action :around  (action application
				 (session transactional-session-mixin)
				 (frame transactional-session-frame-mixin))
  (if-bind transaction (car (frame.transaction-stack frame))
    (if (eql transaction :open)
	(call-next-method)
	(progn
	  (setf (context.current-frame *context*) (cdr transaction))
	  (ucw-core::make-new-frame action (context.session *context*))
	  (with-call/cc (funcall (car transaction) t))))
    (call-next-method)))

;;; export? This name would conflict with pretty much every database
;;; package ever...
(defmacro with-transaction ((&rest options) &body body)
  (declare (ignore options))
  `(progn (open-transaction)
	  ,@body
	  (close-transaction)))