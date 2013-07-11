(in-package :ucw-user)

(defvar *example-server* 
  (make-instance 'standard-server))

(defun start-example-server (&key 
			     (backend :httpd) 
			     (port 8000))
  (if (server.backend *example-server*)
      (error "Server already started")
      (setf (server.backend *example-server*)
	    (make-backend backend :port port)))  
  (startup-server *example-server*))

(defclass example-application (standard-application 
			       cookie-session-application-mixin)
  ()
  (:default-initargs
    :url-prefix "/example/"))

(defvar *example-application* (make-instance 'example-application))

(defentry-point "number" (:application *example-application* :with-call/cc nil
				       :action-options (:call-render nil)) ((number "42"))
 (format (html-stream (context.response *context*)) "~@R" (parse-integer number :junk-allowed t)))

(defentry-point "index.ucw" (:application *example-application*) () (call 'example-message))

(register-application *example-server* *example-application*)

(defentry-point "hello.ucw" (:application *example-application*
			     :with-call/cc nil) 
    ()
  (format (html-stream (context.response *context*))
	  "Hello World"))

(Defentry-point "hello2.ucw" (:application *example-application*
			     :with-call/cc nil) 
    ((message "World"))
  (format (ucw-core::html-stream (ucw-core::context.response *context*))
	  "Hello ~A" message))


(defun render-page-wrapper (title thunk) 
  (<:html 
   (<:head (<:title (<:as-html title)))
   (<:body :id "body" (funcall thunk))))

(defun render-message (message)
  (<:span 
   :class "message" 
   (<:as-html "Hello " message "!")))

(defun render-message-form (name)
  (<:form 
   (<:as-html "Enter a new message:")
   (<:input 
    :type "text" :name (string-downcase name))
   (<:submit)))

(defentry-point "hello-yaclml.ucw" 
    (:application *example-application* 
     :with-call/cc nil) 
    ((message nil))
  (render-page-wrapper "UCW Example" 
    (lambda () 
      (<:style ".message {font-size:2em;font-weight:bold")
      (if message 
	  (render-message message)
	  (render-message-form 'message)))))

(defclass example-message ()
  ((message :accessor message :initarg :message :initform "World!"))
  (:metaclass standard-component-class))

(defmethod render :before ((self example-message))
  (<:style ".message {font-size:2em;font-weight:bold"))

(defmethod render ((self example-message))
  (render-message (message  self)))

(defclass example-form ()
  ()
  (:metaclass standard-component-class))

(defmethod render ((self example-form))
  (render-message-form 'message))


(defclass example-window (window-component)
  ((title :accessor window.title 
	  :initform "UCW Example"
	  :initarg :title)
   (body :accessor window.body
	 :component t
	 :initarg :body))
  (:metaclass standard-component-class))

(defmethod render :around ((window example-window))
  (render-page-wrapper (window.title window)
    (lambda () (call-next-method))))

(defmethod render ((window example-window))
  (render (window.body window)))

(defun render-example-window (body-component-name 
			      &rest initargs)
  (render 
   (make-instance 'example-window  
    :body (apply #'make-instance body-component-name initargs))))

(defentry-point "hello-components.ucw" 
    (:application *example-application*) 
    ((message nil))
  (if message 
      (render-example-window 'example-message :message message)
      (render-example-window 'example-form)))


(defmethod dump-object-to-html ((object t))
  (<:as-html (format nil "~A"  object)))

(defmethod dump-object-to-html ((object standard-object))
  (let ((class (class-of object)))
    (<:table :border 1
     (<:big (<:as-html (class-name class)))
     (loop 
      :for slotd :in (c2mop:class-slots class)
      :do (<:tr 
	   (<:td 
	    (<:strong  
	     (dump-object-to-html (c2mop:slot-definition-name slotd))))
	   (<:td (dump-object-to-html 
		  (c2mop::slot-value-using-class 
		   class object slotd))))))))


(define-symbol-macro $message (get-session-value :message))

(defmethod render-reset-link (component)
  (<:div (<:a :href "hello-session.ucw?reset"
	      (<:as-html "Reset Message"))))


(defclass example-session-message (example-message)
  ()
  (:metaclass standard-component-class))

(defmethod message ((self example-session-message))
  $message)

(defmethod render :after ((self example-session-message))
  (render-reset-link self))


(defclass example-session-form (example-form)
  ()
  (:metaclass standard-component-class))


(defentry-point "hello-session.ucw" 
    (:application *example-application*
     :with-call/cc nil) 
    ((message nil) (reset nil reset-requested-p))

  (when message 
    (setf $message message))

  (when reset-requested-p
    (setf $message nil))
  
  (if $message 
      (render-example-window 'example-session-message)
      (render-example-window 'example-session-form)))


(defclass entry-point-action-mixin ()
  ((entry-point :accessor entry-point 
		:initarg :entry-point 
		:initform "hello-action.ucw")))

(defmethod compute-url :around (action (component entry-point-action-mixin))
  (let ((url (call-next-method)))

    (setf (uri.path url) 
	  (format nil "~A~A" (uri.path url) (entry-point component)))
    url))

(defun go-to (body-component-name 
		       &rest initargs)
  (setf (frame.window-component 
	 (context.current-frame *context*))
	(make-instance 'example-window  
		       :body (apply #'make-instance 
				    body-component-name 
				    initargs))
))

(defclass action-reset-link-mixin () ())


(defmethod reset-message (component)
  (setf $message nil)
  (go-to 'example-form)) 

(defmethod render-reset-link ((self action-reset-link-mixin))
  (let* ((action (register-action (:with-call/cc nil)
		   (reset-message self)))
	 (url (compute-url action self)))

    (<:a :href (print-uri-to-string url) "Reset Message")))

(defclass example-action-message (example-session-message
				  entry-point-action-mixin
				  action-reset-link-mixin)
  ()
  (:metaclass standard-component-class))

(defentry-point "hello-action.ucw" 
    (:application *example-application*
     :with-call/cc nil) 
    ((message nil))

  (when message 
    (setf $message message))
  
  (if $message 
      (go-to 'example-action-message)
      (go-to 'example-session-form)))




(defclass example-callback-form (example-form)
  ()
  (:metaclass standard-component-class))

(defmethod render-input (component)
  (let ((callback (register-callback 
		   (lambda (value)
		     (setf $message value)))))
    (<:input 
     :type "text"
     :name callback)))

(defmethod render-submit (component)
  (<:submit))

(defmethod render ((self example-callback-form))
 (<ucw:form 
   :function (lambda ()
	       (when $message 
		 (go-to 'example-callback-message)))
   (<:as-html "Enter a new message:")
   (render-input self)
   (render-submit self)))

(defclass example-callback-message (example-session-message
				    action-reset-link-mixin
)
  ()
  (:metaclass standard-component-class))

(defmethod reset-message ((component example-callback-message))
  (setf $message nil)
  (go-to 'example-callback-form))

(defentry-point "hello-callback.ucw" 
    (:application *example-application*
     :with-call/cc nil) ()
  
  (if $message 
      (go-to 'example-callback-message)
      (go-to 'example-callback-form)))

(defclass example-control-flow-form (example-form)
  ((prompt :accessor prompt 
	   :initarg :prompt 
	   :initform "Enter a new message:"))
  (:metaclass standard-component-class))

(defmethod render ((self example-control-flow-form))
  (let ((input ""))
    (<ucw:form 
     :action (answer input)
     (<:as-html (prompt self))
     (<ucw:input :type "text" :accessor input)
     (render-submit self))))

(defclass example-control-flow-message (example-message) ()
  (:metaclass standard-component-class))

(defmethod render :after ((self example-control-flow-message))
  (<:div (<ucw:a :action (answer nil) "Reset message")))

(defaction display-message (message)
  (unless (call 'example-control-flow-message :message message)
    (setf $message nil)))

(defaction get-message-from-user ()
  (setf $message (call 'example-control-flow-form)))

(defentry-point "hello-control-flow.ucw" 
    (:application *example-application*) ()
  (loop 
   (unless $message 
     (get-message-from-user))
   (display-message $message)))












