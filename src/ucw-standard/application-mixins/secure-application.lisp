;; See the file LICENCE for licence information.
(in-package :ucw-standard)

(defconstant +user-session-key+
  '%user%
  "Session key for user's authorization data.")

(defun session-user (&optional (session (context.session *context*)))
  (%session-user session))

(defsetf session-user (&optional session) (new)
  `(setf (%session-user (or ,session
                            (context.session *context*))) ,new))

(defun session-authenticated-p (&optional (session (context.session *context*)))
  (%session-user session))

(defmethod %session-user ((self basic-session))
  (get-session-value +user-session-key+ nil self))

(defmethod (setf %session-user) (value (self basic-session))
  (setf (get-session-value +user-session-key+ self) value))

(defmethod %session-user ((self null))
  nil)

(defmethod (setf %session-user) (value (self null))
  nil)

(defclass secure-application-mixin ()
  ((login-component :accessor login-component
                    :initarg :login-component
                    :initform 'user-login-window
                    :documentation
                    "Class of component for user login.
It must return authenticated USER data or NIL on answer.")
   (insecure-components :accessor insecure-components
                        :initarg :insecure-components
                        :initform '(error-message-window)
                        :documentation
                        "Calls to these components are never checked."))
  (:documentation 
   "Mixin class for applications which require authorized access.
Concrete application must specialize the following methods:
APPLICATION-FIND-USER (APPLICATION USERNAME)
APPLICATION-CHECK-PASSWORD (APPLICATION USER PASSWORD)
APPLICATION-AUTHORIZE-CALL (APPLICATION USER FROM-COMPONENT TO-COMPONENT)."))

(defgeneric secure-application-p (application)
  (:method ((app secure-application-mixin))
    (declare (ignore app))
    t)
  (:method ((app t))
    (declare (ignore app))
    nil)
  (:documentation "Does APPLICATION require authorized access."))

(defgeneric application-find-user (application username)
  (:method ((app application) username)
    (declare (ignore app))
    username)
  (:documentation "Find USER by USERNAME for APPLICATION."))

(defun find-application-user (username
			      &optional (app (context.application *context*)))
  (application-find-user app username))

(defgeneric application-check-password (application user password)
  (:method ((app application) user password)
    (declare (ignore app user password))
    nil)
  (:documentation "Validate PASSWORD against USER for APPLICATION."))

(defun check-user-password (user password
                            &optional (app (context.application *context*)))
  (application-check-password app user password))

(defgeneric/cc application-authenticate-user (application)
  (:documentation "Ask user for credentials, authenticate and return
valid USER data or NIL. "))

(defmethod/cc application-authenticate-user ((app secure-application-mixin))
  (let ((user-pass (call-as-window (login-component app))))
    (let ((user (application-find-user app (car user-pass))))
      (if (and user (application-check-password app user (cdr user-pass)))
	  user))))

(defmethod/cc application-authenticate-user ((app t))
  nil)

(defgeneric/cc application-authorize-session (application &optional session))

(defmethod/cc application-authorize-session ((app t) &optional session)
  nil)

(defmethod/cc application-authorize-session
    ((app secure-application-mixin)
     &optional (session (context.session *context*)))
  (setf (session-user session) (application-authenticate-user app)))

(defgeneric application-unauthorize-session (application &optional session)
  (:documentation "Clear session user data.")
  (:method ((app t) &optional session)
    (declare (ignore app session))
    nil)
  (:method ((app secure-application-mixin)
            &optional (session (context.session *context*)))
    (declare (ignore app))
    (setf (%session-user session) nil)))

(defgeneric application-authorize-call (application user from to)
  (:documentation "Check user credentials to call TO by FROM in APPLICATION.")
  (:method ((app secure-application-mixin) user from to)
    (declare (ignore app from to))
    user)
  (:method :around ((app secure-application-mixin) user from to)
    (declare (ignore user from))
    (let ((insecure (cons (login-component app)
                          (insecure-components app))))
      (if (member (type-of to) insecure)
          t
          (call-next-method))))
  (:method ((app t) user from to)
    (declare (ignore app from to user))
    t))

(defmethod/cc on-authorization-reject
    ((app application) user (from t) (to standard-component))
  (call-as-window 'error-message-window :message "Access denied."))

(defmethod/cc call-component :before ((from t) (to standard-component))
  (let ((app (context.application *context*)))
    (unless (or (application-authorize-call app (session-user) from to)
                (and (not (session-authenticated-p))
                     (application-authorize-session app)
                     (application-authorize-call app (session-user) from to)))
      (on-authorization-reject app (session-user) from to))))

(defmethod/cc login-user ((self standard-component))
  (application-authorize-session (context.application *context*)))

(defmethod/cc logout-user ((self standard-component))
  "Delete user but stay at the same place."
  (let ((app (context.application *context*)))
    (when (typep app 'secure-application-mixin)
      (application-unauthorize-session app))))

(defmethod/cc exit-user ((self standard-component))
  "Delete session and throw user away to entry point."
  (let ((app (context.application *context*)))
    (mark-session-expired (context.session *context*))
    (call-as-window 'redirect-component
		    :target (query-path (context.request *context*)))))

;;; USER LOGIN LOGOUT TRACK MODULE
(defclass user-track-session-mixin ()
  ())

(defclass user-track-application-mixin ()
  ((online-users :accessor application.online-users :initarg :online-users :initform '()))
  (:documentation "module to mix with your application in order to track logged in and out users."))

(defmethod delete-session :after (application (self user-track-session-mixin))
  (when (boundp '*context*)
    (awhen (session-user)
      (setf (application.online-users (context.application *context*))
            (remove it (application.online-users (context.application *context*)))))))

(defmethod (setf %session-user) :before (value (self user-track-session-mixin))
  (when (boundp '*context*)
    (awhen (%session-user self)
      (setf (application.online-users (context.application *context*))
            (remove it (application.online-users (context.application *context*)))))
    (when value
      (pushnew value (application.online-users (context.application *context*))))))

(defmethod session-class list ((app user-track-application-mixin))
  'user-track-session-mixin)
