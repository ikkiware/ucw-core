(in-package :ucw-standard)

(defclass static-roots-application-mixin (application)
  ((static-roots :accessor application.static-roots
		 :initarg :static-roots
		 :initform nil
		 :documentation "A list of (URL-subdir . pathname)
              pairs to use when looking for static files. This follows
              standard CL pathname syntax i.e. you must include a
              trailing / in both the URL-subdir and pathname.")))



(defmethod startup-application :after ((app static-roots-application-mixin))
  (when-bind static-roots (application.static-roots app)
    (mapc (lambda (root)
	    (defentry-point (car root) (:application app
				        :class starts-with-dispatcher
					:with-call/cc nil
					:action-options (:class 'action))
		()
	      (serve-file (merge-pathnames *dispatcher-url-suffix*
					   (cdr root)))))
	  static-roots)))