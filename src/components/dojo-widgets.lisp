;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

;; TODO better integrate ucw dirty checking and validation with dojo

(enable-sharpquote<>-syntax)
(enable-bracket-syntax)

(defmacro rendering-dojo-tooltip-for (id &body body)
  "This macro will bind the html rendered in its body to the dom node given by ID as a tooltip."
  `(<:span (@ "dojo:type" "tooltip" "dojo:connectid" ,id) :style "display: none"
           ,@body))

;; TODO move to parenscript? name?
(defun to-js-boolean (value)
  (if value 'true 'false))

;;; This file contains some dojo widget wrappers. They are basically CLOS objects
;;; representing dojo widgets with their parameters.

(defmacro def-dojo-widget (name supers slots &rest args)
  (flet ((find-arg (name)
           (find name args :key #'car))
         (delete-arg (arg)
           (setf args (delete arg args :test #'eq))))
    (let ((default-initargs (find-arg :default-initargs))
          (dojo-type)
          (metaclass (find-arg :metaclass)))
      (setf args (copy-list args))
      (if (listp name)
          (setf dojo-type (second name)
                name (first name)))
      (if default-initargs
          (progn
            (delete-arg default-initargs)
            (setf default-initargs (copy-list default-initargs)))
          (setf default-initargs (list :default-initargs)))
      (if metaclass
          (delete-arg metaclass)
          (setf metaclass (list :metaclass 'standard-component-class)))
      (when dojo-type
        (setf (getf (rest default-initargs) :dojo-type) dojo-type))
      (unless (find 'dojo-widget supers)
        (setf supers (append supers (list 'dojo-widget))))
      `(defcomponent ,name ,supers ,slots
        ,@(when (> (length default-initargs) 1)
                (list default-initargs))
        ,metaclass
        ,@args))))

(defcomponent dojo-widget (widget-component html-element)
  ((dojo-type :accessor dojo-type-of :initarg :dojo-type)
   (widget-id :accessor widget-id))
  (:documentation "An abstract dojo widget that does not render anything."))

(defmethod (setf dom-id) :after (value (self dojo-widget))
  (setf (widget-id self) (strcat value "-widget")))

(defmethod rendered-form-fields ((self dojo-widget))
  (list (widget-id self)))

(defmethod initialize-instance :after ((self dojo-widget) &key)
  ;; trigger our specialized method above
  (setf (dom-id self) (dom-id self))
  (setf (dojo-type-of self) (dojo-type-of self)))

(defcomponent simple-dojo-widget (dojo-widget)
  ()
  (:documentation "A dojo widget which should be wrapped in a <div> with a dojoType=\"...\" attribute."))

(defmacro with-dojo-widget-tag ((widget &rest args) &body body)
  ;; if we started ajax rendering from here then do not render the dojo div, because it's
  ;; against the dojo contract to replace those dom nodes. rather render the body div, so
  ;; the client side will only replace that dom node...
  ;; TODO currently-ajax-rendered-component should be dropped if dijit rendering does not need it
  `(if (eq (currently-ajax-rendered-component) ,widget)
       (progn
         ,@body)
       (<:div :id (widget-id ,widget)
              (@ "dojo:type" (dojo-type-of ,widget) ,@args)
        ,@body)))

(defmethod render-widget-wrapper :around ((self simple-dojo-widget) next-render-method)
  "Wrap the simple dojo widget in a single <div dojoType=\"...\"> tag."
  (<:div :class (css-class self) :id (widget-id self) :style (css-style self)
         (@ "dojo:type" (dojo-type-of self))
         (funcall next-render-method)))

(defmethod render ((self simple-dojo-widget))
  (<:div :id (widget-id self)
         (@ "dojo:type" (dojo-type-of self))))

;;;
;;; dojo-content-pane - ContentPane
;;;
(def-dojo-widget (dojo-content-pane "ContentPane") ()
  ((body :initform nil :initarg :body :accessor body-of :component t)))

(defmethod render ((self dojo-content-pane))
  (with-dojo-widget-tag (self)
    (awhen (body-of self)
      (etypecase it
        (function (funcall it))
        (component (render it))))))

;;;
;;; dojo-tab-container - TabContainer
;;;
(def-dojo-widget (dojo-tab-container "TabContainer") (switching-container)
  ((do-layout-p :initform nil :initarg :do-layout-p :accessor do-layout-p)
   (remember-selected-tab-p :initform nil :initarg :remember-selected-tab-p :accessor remember-selected-tab-p))
  (:default-initargs :forbid-ajax-rendering-p t :client-side-p t))

(defmethod render-widget-wrapper :around ((self dojo-tab-container) next-render-method)
  ;; we can't use with-dojo-widget-tag because TabContainer does not tolerate extra levels in its body
  (<:div :id (dom-id self) :style (css-style self) :class (css-class self)
         (@ "dojo:widgetId" (widget-id self)
            "dojo:type" (dojo-type-of self)
            "dojo:doLayout" (to-js-boolean (do-layout-p self))
            "dojo:selectedChild" (awhen (container.current-component self)
                                   (widget-id it))
            "dojo:postInitialize" (when (remember-selected-tab-p self)
                                    (js:js* `(ucw.widget.tab-container.setup-remember-selected-tab
                                              ,(widget-id self)))))
         (funcall next-render-method)))

(defmethod render ((self dojo-tab-container))
  (iter (for (nil . tab) in (container.contents self))
        ;; TODO selected tabs shouldn't be rendered in a second request
        (render-ajax-stub tab)))

;;;
;;; dojo-tab
;;;
(def-dojo-widget dojo-tab (list-container dojo-content-pane)
  ((label :initform nil :initarg :label :accessor label-of)
   (closablep :initform t :initarg :closablep :accessor closablep))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-tab")))

(defmethod render-widget-wrapper :around ((self dojo-tab) next-render-method)
  (with-dojo-widget-tag (self "dojo:label" (label-of self)
                              "dojo:closable" (to-js-boolean (closablep self))
                              "dojo:postInitialize"
                              (js:js* `(progn
                                        (log.debug "Setting up content loader of tab " this)
                                        (.set-handler
                                         (dojo.widget.by-id ,(widget-id self))
                                         (lambda (pane node)
                                           (log.debug "AJAX-getting tab pane " pane)
                                           (ucw.io.execute-ajax-action
                                            (create :url
                                                    ,(action-href
                                                      (register-ajax-action (:with-call/cc nil :make-new-frame nil)
                                                        (within-dom-replacements-tag
                                                          (ajax-render self))))
                                                    :forms-to-ask (array)
                                                    :progress-label ,#"progress-label.loading-tab"))))
                                        (log.debug "Setting up on-close of tab " this)
                                        ,(when (closablep self)
                                          `(dojo.event.connect this "onClose"
                                            (lambda ()
                                              (log.debug "Calling server to close the tab " ,(dom-id self))
                                              ,(js-to-lisp-rpc* (:progress-label #"progress-label.closing-tab"
                                                                 :sync false)
                                                                ()
                                                 (awhen (parent self)
                                                   (without-dirtyness-tracking
                                                     (unless (remove-component it (funcall (container.key-generator it) self))
                                                       (ucw.component.warn "Tab ~S was not found in the container when the close server callback was called" self)))))
                                              (return true)))))))
    (call-next-method)))

(defmethod ajax-render-new-tab ((self dojo-tab) &key (select t))
  (within-xhtml-tag "tabs"
    (render-ajax-stub self))
  (<ucw:script `(let ((tab-adder (ucw.io.make-node-walking-ajax-answer-processor "tabs"
                                  (lambda (node original-node)
                                    (log.debug "Processing a tab: " node.tag-name " with id " node.id)
                                    (let ((tab (dojo.widget.create-widget node))
                                          (container (dojo.widget.by-id ,(widget-id (parent self)))))
                                      (assert (= tab (dojo.widget.by-id ,(widget-id self))))
                                      (log.debug "Adding tab " tab.widget-id " to container " container.widget-id)
                                      (.add-child container tab)
                                      (assert (= tab.parent container))
                                      ,(when select
                                             `(.select-child container tab)))
                                    ;; we disable postprocess-inserted-node by returning false
                                    ;; because we already instantiated the widgets with the
                                    ;; dojo.widget.create-widget above. but then we need to eval
                                    ;; the script tags ourselves.
                                    (ucw.io.eval-script-tags original-node)
                                    (return false)))))
                 (tab-adder nil current-ajax-answer nil))))

(defun wrap-in-dojo-tab (component &rest args &key label closablep &allow-other-keys)
  (declare (ignore label closablep))
  (let ((tab (apply #'make-instance 'dojo-tab args)))
    (add-component tab component)
    tab))

;;;
;;; dojo-split-container - SplitContainer
;;;
(def-dojo-widget (dojo-split-container "SplitContainer") (list-container)
  ((sizer-width :initform 5 :initarg :sizer-width :accessor sizer-width-of)
   (active-sizing-p :initform nil :initarg :active-sizing-p :accessor active-sizing-p))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-split")
    :orientation :horizontal))

(defmethod render-widget-wrapper :around ((self dojo-split-container) next-render-method)
  (with-dojo-widget-tag (self "dojo:orientation" (if (eq (orientation self) :horizontal)
                                                     "horizontal"
                                                     "vertical")
                              "dojo:sizerwidth" (sizer-width-of self)
                              "dojo:activesizing" (to-js-boolean (active-sizing-p self)))
    (call-next-method)))

(defmethod render ((self dojo-split-container))
  (iter (for (nil . c) in (container.contents self))
        (render c)))

;;;
;;; dojo-editor - Editor
;;;
(def-dojo-widget (dojo-html-text-editor "Editor2") (dojo-widget ; it's more important, make sure it's first
                                                    textarea-field)
  ((min-height :initform nil :accessor min-height-of :initarg :min-height)))

(defmethod render ((self dojo-html-text-editor))
  ;; we can't use with-dojo-widget-tag because dojo-editor needs a <:textarea
  (<ucw:textarea :id (dom-id self)
                 :class (css-class self)
                 :style (css-style self)
                 :name (name self)
                 :accessor (client-value self)
                 :rows (rows self)
                 :cols (cols self)
                 :title (tooltip self)
                 :tabindex (tabindex self)
                 (@ "dojo:widgetId" (widget-id self)
                    "dojo:type" (dojo-type-of self)
                    "dojo:minHeight" (min-height-of self))
                 (funcall next-render-method)))

;;;
;;; Date and time
;;;
(def-dojo-widget dojo-dropdown (simple-dojo-widget)
  ((effect :initform "fade" :initarg :effect :accessor effect-of)))

(defcomponent local-time-based-dojo-input-widget (dojo-widget
                                                  local-time-based-date-field
                                                  generic-html-input)
  ())

(def-dojo-widget (dojo-date-picker "DatePicker") (local-time-based-dojo-input-widget)
  ((display-format :initarg :display-format :accessor display-format-of))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-date")))

(def-dojo-widget (dojo-time-picker "TimePicker") (local-time-based-dojo-input-widget)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-time")))

(defcomponent local-time-based-dojo-dropdown-widget (local-time-based-dojo-input-widget
                                                     dojo-dropdown)
  ())

(def-dojo-widget (dojo-dropdown-date-picker "DropdownDatePicker") (local-time-based-dojo-dropdown-widget
                                                                   dojo-date-picker)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-dd-date")))

(def-dojo-widget (dojo-dropdown-time-picker "DropdownTimePicker") (local-time-based-dojo-dropdown-widget
                                                                   dojo-time-picker)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-dd-time")))

(defun format-local-time-for-dojo (local-time &rest args)
  (apply #'format-rfc3339-timestring local-time :use-zulu-p nil args))

(defmethod (setf value) ((local-time local-time) (self dojo-dropdown-date-picker))
  (setf (client-value self) (format-local-time-for-dojo local-time)))

(defmethod (setf value) ((local-time local-time) (self dojo-dropdown-time-picker))
  (setf (client-value self) (format-local-time-for-dojo local-time)))

(defmethod value ((self dojo-dropdown-date-picker))
  (let ((client-value (client-value self)))
    (when (and client-value
               (not (zerop (length client-value))))
      (minimize-time-part (parse-timestring client-value :allow-missing-time-part-p t)))))

(defmethod value ((self dojo-dropdown-time-picker))
  (let ((client-value (client-value self)))
    (when (and client-value
               (not (zerop (length client-value))))
      ;; HINT: the server's *default-timezone* shoud be +utc-zone+ to minimize headaches
      (parse-timestring client-value :allow-missing-date-part-p t))))

(defmethod render-widget-wrapper :around ((self local-time-based-dojo-dropdown-widget) next-render-method)
  (with-html-element-wrapper self <:div
    (funcall next-render-method)))

(defmethod render ((self local-time-based-dojo-dropdown-widget))
  (<ucw:input :id (widget-id self)
              :class (css-class self)
              :style (css-style self)
              :accessor (client-value self)
              :accesskey (accesskey self)
              :title (tooltip self)
              :tabindex (tabindex self)
              ;; NOTE: dojo drops this dom node with all its attributes. some of them are copied though...
              (@ "dojo:type" (dojo-type-of self)
                 "dojo:inputId" (dom-id self)
                 "dojo:displayFormat" (when (and (typep self 'dojo-dropdown-date-picker)
                                                 (slot-boundp self 'display-format))
                                        (display-format-of self))
                 "dojo:value" (client-value self)
                 "dojo:containerToggle" (effect-of self))))

(defmethod javascript-init ((field dojo-date-picker) (validator time-range-validator))
  ;; set the dojo dddp's valid range
  (let ((min-value (min-value validator))
        (max-value (max-value validator)))
    `(on-load
      (let ((field (dojo.widget.by-id ,(widget-id field)))
            (date-picker))
        (cond
          ((instanceof field dojo.widget.*dropdown-date-picker) (setf date-picker field.date-picker))
          ((instanceof field dojo.widget.*date-picker) (setf date-picker field))
          (t (let ((message (+ "Unexpected date widget type '" field.widget-type "'")))
               (log.error message field)
               (throw message))))
        ;; TODO this is messing with dojo internals and will probably break in the future.
        ;; we should call a set-start-date method if one were available...
        ,(when min-value
               `(setf date-picker.start-date (dojo.date.from-rfc3339
                                              ,(format-local-time-for-dojo min-value :omit-timezone-part-p t))))
        ,(when max-value
               `(setf date-picker.end-date (dojo.date.from-rfc3339
                                            ,(format-local-time-for-dojo max-value :omit-timezone-part-p t))))
        ,(when (or min-value max-value)
               `(date-picker._init-u-i 42))))))

(defcomponent dojo-timestamp-picker (generic-html-input)
  ((date-picker :accessor date-picker-of :component (dojo-dropdown-date-picker))
   (time-picker :accessor time-picker-of :component (dojo-dropdown-time-picker))
   (value :initform nil :initarg :value)))

(defmethod shared-initialize :after ((self dojo-timestamp-picker) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (value self) (value self)))

(defmethod rendered-form-fields ((self dojo-timestamp-picker))
  (list (widget-id (date-picker-of self))
        (widget-id (time-picker-of self))))

(defmethod value ((self dojo-timestamp-picker))
  (unless (slot-value self 'value)
    (let ((date (value (date-picker-of self)))
          (time (value (time-picker-of self))))
      ;; TODO does the timezone stuff correctly come up to the server?
      (if date
          (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p timezone)
              (decode-local-time date)
            (declare (ignore day-of-week daylight-saving-time-p))
            (if time
                (multiple-value-setq (usec sec min hour) (decode-local-time time))
                (setf usec 0
                      sec 0
                      min 0
                      hour 0))
            (setf (slot-value self 'value)
                  (encode-local-time usec sec min hour day month year :timezone timezone)))
          (setf (slot-value self 'value) nil))))
  (slot-value self 'value))

(defmethod (setf value) ((local-time local-time) (self dojo-timestamp-picker))
  (setf (value (date-picker-of self)) local-time)
  (setf (value (time-picker-of self)) local-time))

(defmethod (setf value) ((time null) (self dojo-timestamp-picker))
  (setf (value (date-picker-of self)) nil)
  (setf (value (time-picker-of self)) nil))

(defmethod render :before ((self dojo-timestamp-picker))
  (setf (slot-value self 'value) nil))

(defmethod render ((self dojo-timestamp-picker))
  (<:table
      (<:tr
       (<:td (render (date-picker-of self)))
       (<:td (render (time-picker-of self))))))

;; TODO this widget is missing the ucw.field.register call because it's not a generic-html-input (to avoid rendering script tags into its body)
;; because of this it'll not be posted when used inside a form
(def-dojo-widget (dojo-inline-edit-box "InlineEditBox") (dojo-widget html-input)
  ((tag-name
    :initform "p"
    :initarg :tag-name
    :accessor tag-name-of)
   (mode
    :initform nil
    :initarg :mode
    :accessor mode-of)
   (value
    :initarg :value
    :accessor value))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "djinlebox")))

(defmethod render-widget-wrapper :around ((self dojo-inline-edit-box) next-render-method)
  {with-xml-syntax
    <(tag-name-of self) (@ "dojo:type" "InlineEditBox"
                           "dojo:mode" (mode-of self))
                        :class (css-class self)
                        :id (widget-id self)
                        :style (css-style self)
       (funcall next-render-method)>}
  (<ucw:script :toplevelp t
               `(on-load
                 (let ((edit-box (dojo.widget.by-id ,(widget-id self))))
                   (dojo.event.connect edit-box "onSave"
                                       (lambda (new-value old-value name)
                                         ,(js-to-lisp-rpc* () (new-value)
                                            (ucw.component.dojo.dribble "Changing value of ~A to ~S" self new-value)
                                            (setf (value self) new-value)
                                            (values))))))))

(defmethod render ((self dojo-inline-edit-box))
  (<:as-html (value self)))

