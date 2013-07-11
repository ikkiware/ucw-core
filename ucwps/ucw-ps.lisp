(in-package :ucw-standard)



;;;UCW tag
;;;For <ucw:script and :javascript (:js form) revival 
(eval-always

(deftag-macro <ps:script (&attribute (compile-time-p nil) (toplevelp nil)
                                      &allow-other-attributes others
                                      &body body)
  (unless toplevelp
    (setf body (list ``((lambda ()
                          ,,@body)))))
  `(<:script :type "text/javascript"
             ,@others
             (<:as-is #\Newline "// <![CDATA[" #\Newline
                      ,(if compile-time-p
                           (iter (for expr in body)
                                 (collect (ps:ps* (eval expr)) :into result)
                                 (finally (return (apply #'concatenate 'string result))))
                           `(ps:ps* ,@body))
                      #\Newline "// ]]>" #\Newline)))
)




;;;UCW Component
;;; ps-window-features-mixin
(defcomponent ps-window-features-mixin (basic-window-features-mixin)
  ((javascript :accessor window-component.javascript
               :initarg :javascript
               :initform nil
               :documentation "List of javascript includes.

Each element must be a list whose first value is either the
symbol :SRC or :JS.

 (:SRC url) - writes <script src=\"URL\"></script> tag.
 (:JS form) - equivalent to (:SCRIPT (js:js* form))
 (:SCRIPT string) - write <script>STRING</script>.

The elements will be rendered in order."))
  (:documentation "A mixin that renders basic and parenscript/javascript html toplevel tags."))


(defmethod render-window-scripts ((window ps-window-features-mixin))
    (dolist* ((type value) (window-component.javascript window))
      (ecase type
	(:src
	 (<:script :type "text/javascript"
		   :src value
		   ;; most browsers (firefox, safari and ie at least) really,
		   ;; really, really don't like empty script tags. The "" forces
		   ;; yaclml to generate a separate closing tag.
		   ""))
	(:js
	      (<ps:script :toplevelp t
			   (if (functionp value)
			       (funcall value)
			       value)))
	(:script
	 (<:script :type "text/javascript"
		   (<:as-is ~% "// <![CDATA[" ~%
			    value
			    ~% "// ]]>" ~%))))))

;;;ps-basic-window-component
(defcomponent ps-basic-window-component (ps-window-features-mixin window-component)
  ()
  (:documentation "A convenience class for writing window components with parenscript embedded support."))


;;;ps-standard-window-component
(defcomponent ps-standard-window-component  (ps-basic-window-component)
  ((body
    :initform nil
    :accessor window-body
    :component t
    :initarg :body)))


#+nil(defmethod render-html-head ((window ps-standard-window-component))
  (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
  (awhen (window-component.title window)
    (<:title (if (functionp it)
		 (funcall it window)
		 (<:as-html it))))
  (awhen (window-component.icon window)
    (<:link :rel "icon"
	    :type "image/x-icon"
	    :href it))
  (dolist (stylesheet (effective-window-stylesheets window))
    (<:link :rel "stylesheet"
	    :href stylesheet
	    :type "text/css")))


(defmethod render-html-body ((window ps-standard-window-component))
  (render (window-body window)))



(export (find-symbol (symbol-name :ps-standard-window-component)  :ucw))
