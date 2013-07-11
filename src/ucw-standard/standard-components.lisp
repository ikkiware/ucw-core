(in-package :ucw-standard)

(defmethod render :wrap-around (component)
  (let ((*source-component* component))
    (call-next-method)))

(defclass standard-application (basic-application)
  ((action-class :accessor application.action-class 
		 :initform 'standard-action
		 :initarg :action-class)))

(defmethod service :around ((app standard-application) context)
  (let ((*default-action-class* (application.action-class app)))
    (call-next-method)))

;;;; ** Simple Window

(defcomponent basic-window-features-mixin ()
  ((title :accessor window-component.title
          :initarg :title
          :initform nil)
   (stylesheet :accessor window-component.stylesheet
               :initarg :stylesheet
               :initform nil
               :documentation "The URL of the css file to use as a stylesheet for this window.")
   (icon :accessor window-component.icon
         :initarg :icon
         :initform nil
         :documentation "Optional URL for an icon.")
   (doctype :accessor window-component.doctype
             :initarg :doctype
             :initform (load-time-value +xhtml-transitional-doctype+)
             :documentation "Doctype for this window.")
   (content-prologue :accessor window-component.content-prologue
                     :initarg :content-prologue
                     :initform nil
                     :documentation "Unless nil it's printed <:as-is before any other output. Suitable for <?xml...?> lines.")
   (html-tag-attributes :accessor window-component.html-tag-attributes
                        :initarg :html-tag-attributes
                        :initform (list "xmlns" #.+xhtml-namespace-uri+)
                        :documentation "A yaclml attribute list that'll be rendered into the <:html tag's attributes.")
   (javascript :accessor window-component.javascript
               :initarg :javascript
               :initform nil
               :documentation "List of javascript includes.

Each element must be a list whose first value is either the
symbol :SRC or :JS.

 (:SRC url) - writes <script src=\"URL\"></script> tag.
 (:JS form) - equivalent to (:SCRIPT (js:js* form))
 (:SCRIPT string) - write <script>STRING</script>.

The elements will be rendered in order."))
  (:documentation "A mixin that renders basic html toplevel tags."))

(defgeneric effective-window-stylesheets (window)
  (:documentation "This method is used to collect the effective stylesheet list for a window; available for customizations.")
  (:method-combination nconc)
  (:method nconc ((thing t))
    (list))
  (:method nconc ((window basic-window-features-mixin))
    (copy-list (window-component.stylesheet window))))

(defmethod render ((window basic-window-features-mixin))
  "This convience method assumes: 1) the stylesheet is
external (as opposed to inlined) or is not used; 2) the script
file is javascript and is external or is no script is used and 3)
the title is either a literal or a lambda with one argument (the
window)."
  (awhen (window-component.content-prologue window)
    (<:as-is it ~%))
  (<:html :doctype (window-component.doctype window)
          (@ (window-component.html-tag-attributes window))
          (render-html-head window)
          (render-html-body window)))

(defgeneric render-html-head (window)
  (:method :around ((window basic-window-features-mixin))
    (<:head (call-next-method)))
  (:method ((window basic-window-features-mixin))
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
	      :type "text/css"))))

(defgeneric render-html-body (window)
  (:method :around ((window basic-window-features-mixin))
           (<:body
            (render-window-scripts window)
            (call-next-method))))

(defgeneric render-window-scripts (window)
  (:method ((window basic-window-features-mixin))
    (dolist* ((type value) (window-component.javascript window))
      (ecase type
	(:src
	 (<:script :type "text/javascript"
		   :src value
		   ;; most browsers (firefox, safari and ie at least) really,
		   ;; really, really don't like empty script tags. The "" forces
		   ;; yaclml to generate a seperate closing tag.
		   ""))
	;; TODO clean up these names
	#+nil(:js
	      (<ucw:script :toplevelp t
			   (if (functionp value)
			       (funcall value)
			       value)))
	(:script
	 (<:script :type "text/javascript"
		   (<:as-is ~% "// <![CDATA[" ~%
			    value
			    ~% "// ]]>" ~%)))))))

(defcomponent basic-window-component (basic-window-features-mixin window-component)
  ()
  (:documentation "A convenience class for writing window components."))


(defcomponent standard-window-component 
  (basic-window-component)
  ((body
    :initform nil
    :accessor window-body
    :component t
    :initarg :body)))

(defmethod render-html-head ((window standard-window-component))
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

(defmethod render-html-body ((window standard-window-component))
  (render (window-body window)))

(defcomponent info-message ()
  ((message :accessor message :initarg :message)))

(defmethod render ((m info-message))
  (<:div
   :class "info-mssage" 
   (<:as-html (message m)))
   (<ucw:a :action (answer-component m nil) "Ok"))