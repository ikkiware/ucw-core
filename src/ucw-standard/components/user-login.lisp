(in-package :ucw-standard)

(defcomponent user-login ()
  ((username :accessor username
             :initarg :username
             :initform nil
             :documentation "User login name.")
   (password :accessor password
             :initarg :password
             :initform nil
             :documentation "User password."))
  (:documentation "User login component."))

(defcomponent user-login-window (basic-window-component)
  ((user-login :initarg :user-login
	       :accessor user-login
	       :component user-login))
  (:documentation "A container window componenet for the user-login.")
  (:default-initargs :title "User Login"))

(defmethod render ((self user-login))
  (<ucw:form :action (refresh-component self)
	     :method "post"
   (<:table
    (<:tr
     (<:td "Username")
     (<:td (<ucw:input :type "text" :accessor (username self))))
    (<:tr
     (<:td "Password")
     (<:td (<ucw:input :type "password" :accessor (password self))))
    (<:tr
     (<:td :colspan 2
	   (<ucw:submit :action (submit self) :value "Ok")
	   (<ucw:submit :action (cancel self) :value "Cancel"))))))

(defmethod render-html-body ((self user-login-window))
  (render (user-login self)))

(defmethod render-html-body :before ((self user-login-window))
  (<:h1  "Please enter login and password."))

(defmethod/cc cancel ((self user-login))
  (answer-component (parent self) nil))

(defmethod/cc submit ((self user-login))
  (answer-component (parent self) (cons (username self) (password self))))
