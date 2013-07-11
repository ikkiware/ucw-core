(in-package :ucw-standard)

(defun make-demo-backend ()
  (make-backend
   :httpd
   :host "localhost"
   :port 9090))

(defclass demo-server (standard-server)
  ())

(defun make-demo-server ()
  (make-instance
   'demo-server
   :backend (make-demo-backend)))

(defvar *demo-ucw-server* (make-demo-server))

(defclass demo-application (standard-application)
  ()
  (:default-initargs
    :url-prefix "/demo/"))

(defparameter *demo-ucw-application* (make-instance 'demo-application))

(register-application *demo-ucw-server* *demo-ucw-application*)

(defentry-point "index.ucw" (:application *demo-ucw-application*) ()
  (call 'demo-window))

(defun startup-demo ()
  (startup-server *demo-ucw-server*))

(defun shutdown-demo ()
 (shutdown-server *demo-ucw-server*))

(defcomponent demo-window (standard-window-component)
  ()
  (:default-initargs 
      :body (make-instance 'demo-component)))

(define-symbol-macro $window (context.window-component *context*))

(define-symbol-macro $body (window-body $window))

(defcomponent demo-component ()
  ((test :component demo-simple-action :accessor test)
   (component :component demo-render :accessor component)))

(define-symbol-macro $test (test $body))

(define-symbol-macro $component (component $body))

(defmethod render ((self demo-component))
  (<:H1 "Lisp On Lines Web test suite")
     (render (slot-value self 'test))
  (<:div 
   :style "border:1px solid black;"
   (render (slot-value self 'component))))

(defcomponent demo-render ()
  ((message :initform "test" :accessor message :initarg :message)))

(defmethod render ((self demo-render))
  (<:h3 :id "test-render" 
	(<:as-html (format nil "Hello ~A." (message self)))))

(defcomponent demo-simple-action ()
  ())

(defmethod render ((self demo-simple-action))
  (<:ul
   (<:li (<ucw:a 
	  :function 
	  (lambda ()
	    (setf (message $component) 
		  (format nil "~A : ~A" (message $component) "FUNCTION")))
	  "Test <:A :FUNCTION type actions"))
   (<:li 
    (<ucw:a 
     :action (setf (message $component) 
		   (format nil "~A : ~A" (message $component) "ACTION"))
     "Test <:A :ACTION type actions"))
   (<:li 
    (<ucw:a 
     :action* (make-action 
	       (lambda ()
		 (setf (message $component) 
		       (format nil "~A : ~A" (message $component) "ACTION*"))))
     "Test <:A :ACTION* type actions"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-answer))
     "Test CALL-COMPONENT/ANSWER-COMPONENT"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-call-magic))
     "Test CALL/ANSWER MAGIC"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-call-answer-action-magic))
     "Test CALL/ANSWER ACTION MAGIC"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-simple-form))
     "Test Simple Form"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-multi-submit-form))
     "Test Multi Form"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'demo-input))
     "Test Form input"))
))

(defcomponent demo-answer (demo-render) ()
  (:default-initargs :message "CALL was ok. Go Back will answer"))

(defmethod render :wrapping ((self demo-answer))
  (call-next-method)
  (<ucw:a :action (answer-component self nil) "Go Back."))

(defcomponent demo-simple-form (demo-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod render :wrapping ((self demo-simple-form))
  (call-next-method)
  (<ucw:form 
   :action (setf (message self) "Form Submitted")
   (<:submit))
  (<ucw:a :action (answer-component self nil) "Go Back."))

(defcomponent demo-multi-submit-form (demo-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod render :wrapping ((self demo-multi-submit-form))
  (call-next-method)
  (<ucw:form 
   :action (setf (message self) "Form Submitted")
   (<:submit)
   (<ucw:submit :action (setf (message self) "Submit 2" )
		:value "2")
   (<ucw:submit :action (setf (message self) "Submit 3")
		3))
  (<ucw:a :action (answer-component self nil) "Go Back."))

(defcomponent demo-input (demo-render) 
 ()	      
  (:default-initargs :message "Testing INPUTS"))

(defmethod render :wrapping ((self demo-input))
  (call-next-method)
  (<ucw:form 
   :function (constantly t)
   (<ucw:input :type "text" :accessor (message self))
   
   (<:submit)
  )
  (<ucw:a :action (answer-component self nil) "Go Back."))



(defcomponent demo-call-magic (demo-render) 
 ()	      
  (:default-initargs :message "Testing CALL magic."))

(defmethod render :wrapping ((self demo-call-magic))
  (call-next-method)
  (<ucw:a :action (setf (message self) (call 'demo-answer-magic)) "Test CALL")
  (<:br)
  (<ucw:a :action (answer-component self nil) "Go Back."))



(defcomponent demo-answer-magic (demo-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(defmethod render :wrapping ((self demo-answer-magic))
  (call-next-method)
  
  (<ucw:a :action (answer "Ja, dat is vut ve answer" ) "IT! (hit here)"))

(defcomponent demo-call-answer-action-magic (demo-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(defaction test-call-component ()
  (call 'demo-call-answer-action-magic :message "We made it"))

(defaction test-answer-component ()
  (answer "We Made IT BACK!!!"))

(defmethod render :wrapping ((self demo-call-answer-action-magic))
  (call-next-method)
  (<ucw:a :action (test-call-component) "Test CALL from ACTION")
  (<:br)  
  (<ucw:a :action (test-answer-component) "Test ANSWER from ACTION"))