(in-package :it.bese.ucw)

(enable-bracket-syntax)

(defvar *ajax-component-being-rendered*)

;; TODO get rid of this if/when the forthcoming dojo refactor makes it pointless
(defun currently-ajax-rendered-component ()
  (when (boundp '*ajax-component-being-rendered*)
    *ajax-component-being-rendered*))

(defun ajax-rendering-in-progress-p ()
  (boundp '*ajax-component-being-rendered*))

;; all subclasses must be standard-component's because the dirtyp slot is needed by the ajax algorithms
(defcomponent ajax-component-mixin (html-element standard-component)
  ((dom-id :initform (js:gen-js-name-string :prefix "ajax")) ; override the initform of the inherited slot
   (has-ever-been-rendered :initform nil :accessor has-ever-been-rendered-p)
   (forbid-ajax-rendering :initform nil :accessor forbid-ajax-rendering-p :initarg :forbid-ajax-rendering-p
                          :documentation "This predicate my forbid AJAX rendering from this component and instruct the renderer to look further on the parent chain. The primary use of this is that sometimes (mostly due to browser rendring bugs) it's better to render bigger chunks of the page."))
  (:documentation "This is a marker class that marks a point in the component
chain from where a partial (AJAX) render may be started. The component
must render exactly one top-level DOM node and it must have an ID attribute.
The client side js will look up the DOM node identified by ID and replace it
with the freshly rendered one.

Please note that this component in itself is not suitable for ajax
DOM node replacements because it does not render any wrapper nodes.
See WIDGET-COMPONENT for an ajax component that works on its own."))

(defparameter %ajax-stub-rendering-in-progress% nil
  "Marks that we are going to render only a stub, so bail out in render :wrapping ajax-component-mixin.")

(defgeneric render-ajax-stub (ajax-component-mixin)
  (:method :around ((self ajax-component-mixin))
           (let ((%ajax-stub-rendering-in-progress% t))
             (call-next-method)))
  (:method ((self ajax-component-mixin))
           (render self))
  (:documentation "Start rendering and stop at ajax-component-mixin boundaries. Only render a stub at those points (usually a <:div with an id) that can be later lazily replaced with an AJAX request."))

(defmethod render :after ((self ajax-component-mixin))
  (setf (has-ever-been-rendered-p self) t))

(defmethod render :wrapping ((self ajax-component-mixin))
  (unless %ajax-stub-rendering-in-progress%
    (call-next-method)))

(defgeneric ajax-render (component)
  (:documentation "This method is called when we are rendering parts of the component hierarchy with AJAX.
By default it simply calls render after marking this fact on the ajax-component-mixin.")
  (:method :around ((self ajax-component-mixin))
           (let ((*ajax-component-being-rendered* self))
             (call-next-method)))
  (:method ((self ajax-component-mixin))
           (render self)))

(defmacro in-restored-rendering-environment (component &body body)
  `(call-in-restored-rendering-environment ,component
                                           (lambda ()
                                             ,@body)))

(defun call-in-restored-rendering-environment (component trunk)
  (let ((parents))
    (iter (for parent :first (parent component) :then (parent parent))
          (while parent)
          (push parent parents))
    (labels ((restorer ()
               ;; this is a nasty trick here: RESTORER is continously passed to the nested calls to
               ;; CALL-IN-RENDERING-ENVIRONMENT until it pop'ped all the parents. then it finally calls
               ;; AJAX-RENDER when all the parents have had a chance to restore the rendering environment.
               (aif (pop parents)
                    (progn
                      (ucw.rerl.ajax.dribble "Calling call-in-rendering-environment for ~S" it)
                      (call-in-rendering-environment #'restorer it))
                    (progn
                      (ucw.rerl.ajax.dribble "Environment of the parents is set up, calling trunk")
                      (call-in-rendering-environment trunk component)))))
      (call-in-rendering-environment #'restorer (pop parents)))))

(defun render-nearest-ajax-component (component)
  (ucw.rerl.ajax.debug "render-nearest-ajax-component from ~S" component)
  (let ((ajax-component (iter (for current :first component :then (parent current))
                              (while current)
                              (ucw.rerl.ajax.dribble "Checking ~S" current)
                              (when (and (typep current 'ajax-component-mixin)
                                         (not (forbid-ajax-rendering-p current)))
                                (return current))
                              (while (slot-boundp current 'parent))
                              (finally (return nil)))))
    (ucw.rerl.ajax.debug "render-nearest-ajax-component ended up at ~S" ajax-component)
    (unless ajax-component
      (error "No suitable ajax-component-mixin was found while walking the parent slots of ~A, unable to render AJAX answer" component))
    ;; we restore the env only up til the parent, because AJAX-RENDER
    ;; calls RENDER which sets up the env of ajax-component itself.
    (aif (parent ajax-component)
         (call-in-restored-rendering-environment it (lambda ()
                                                      (ajax-render ajax-component)))
         (ajax-render ajax-component))))

(define-condition visible-dirty-component-remained (error)
  ((component :initarg :component :accessor component-of))
  (:report (lambda (c stream)
             (format stream "A visible dirty component ~A remained in session ~A after calling ajax-render-dirty-components. This would lead to a constant ajax rerendering in the poller. Make sure you either render all connected components or detach them!"
                     (component-of c) (session-of (component-of c))))))

(defmethod handle-toplevel-condition :around (application
                                              (error visible-dirty-component-remained)
                                              (action ajax-action))
  (when (debug-on-error application)
    (invoke-slime-debugger-if-possible error))
  ;; when we are not debugging, just remove dirtyness and continue normal operation
  (continue))

(defun ajax-render-dirty-components ()
  (within-dom-replacements-tag
    (flet ((render-dirty-ajax-component (component)
             (ucw.rerl.ajax.debug "ajax-render-dirty-components at component ~S" component)
             (render-nearest-ajax-component component)))
      (iterate-visible-dirty-components #'render-dirty-ajax-component))
    (flet ((check-for-remained-dirty-component (c)
             (when (visiblep c)
               (restart-case
                    (error 'visible-dirty-component-remained :component c)
                (continue ()
                  :report "Remove dirtyness and leave me alone..."
                  (setf (dirtyp c) nil))))))
      (iterate-visible-dirty-components #'check-for-remained-dirty-component))))




