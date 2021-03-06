;;; -*- lisp -*-

(in-package :it.bese.ucw)

(enable-bracket-syntax)

;;;; * UCW Extensions to YACLML

(defmacro with-extracted-actions ((arg-list-name &rest action-param-names) &body body)
  "This macro extracts the NAME-id NAME-ajax-p variables, generates the check that only
one of :NAME and :NAME-body attributes may be defined, and in case of a :NAME-body it handles
action registration. The extracted NAME-id and NAME-ajax-p variables are runtime values, so you
can not nil-check them at macroexpand-time, but the compiler can eliminate runtime nil-checks
to constant nil's."
  (flet ((name-generator (symbol appended)
           (intern-concat (list symbol appended)))
         (intern-as-keyword (symbol)
           (intern (symbol-name symbol) (find-package "KEYWORD"))))
    (let* ((param-count (length action-param-names))
           (action-object-tmps (iter (for i below param-count)
                                    (collect (gensym "ACTION-OBJECT"))))
           (action-original-tmps (iter (for i below param-count)
                                      (collect (gensym "ACTION-ORIGINAL"))))
           (action-provided-p-names (mapcar (rcurry #'name-generator "-PROVIDED-P") action-param-names)) ; because we can't nil-check after rebinding
           (action-ajax-p-names (mapcar (rcurry #'name-generator "-AJAX-P") action-param-names))
           (action-ajax-p-tmps (iter (for i below param-count)
                                     (collect (gensym "ACTION-AJAX-P-TMP"))))
           (action-id-names (mapcar (rcurry #'name-generator "-ID") action-param-names))
           (action-id-tmps (iter (for i below param-count)
                                 (collect (gensym "ACTION-ID-TMP"))))
           (action-object-names (mapcar (rcurry #'name-generator "-OBJECT") action-param-names))
           (action-body-names (mapcar (rcurry #'name-generator "-BODY") action-param-names)))
      (with-unique-names ()
        `(progn
          ;; emit the checks for NAME and NAME-body args at the same time
          ,@(iter (for action-param-name in action-param-names)
                  (for action-body-name in action-body-names)
                  (collect `(unless (or (xor ,action-param-name ,action-body-name)
                                     (notany #'identity (list ,action-param-name ,action-body-name)))
                             (error ,(strcat "Only one of " action-param-name " or " action-body-name " is allowed")))))
          ;; remove the NAME and NAME-body keyword args
          ,(when arg-list-name
                 `(remf-keywords ,arg-list-name ,@(mapcar #'intern-as-keyword
                                                          (append action-param-names action-body-names))))
          ,(iter (for action in action-param-names)
                 (for action-object-tmp in action-object-tmps)
                 (for original in action-original-tmps)
                 (for action-body in action-body-names)
                 (for action-object in action-object-names)
                 (for id in action-id-names)
                 (for id-tmp in action-id-tmps)
                 (for provided-p in action-provided-p-names)
                 (for ajax-p in action-ajax-p-names)
                 (for ajax-p-tmp in action-ajax-p-tmps)
                 (nconcing (list #+nil action id ajax-p provided-p) into macro-ignorables)
                 (nconcing (list action-object-tmp id-tmp ajax-p-tmp) into ignorables)
                 (collect `(,provided-p (or ,action ,action-body)) into macro-bindings)
                 (collect `(,original ,action) into macro-backup-bindings)
                 (collect `(,action-object ',action-object-tmp) into macro-bindings)
                 (collect `(,id (and ,provided-p ',id-tmp)) into macro-bindings)
                 (collect `(,ajax-p (and ,provided-p ',ajax-p-tmp)) into macro-bindings)
                 (collect ``(,',action-object-tmp ,(or ,original
                                                      (when ,action-body
                                                        `(register-action (:with-call/cc t)
                                                          (ucw.rerl.info "Executing ~S, body is ~S" ',,action ',,action-body)
                                                          ,,action-body)))) into bindings)
                 (collect ``(,',id-tmp (when ,',action-object-tmp
                                         (action-id ,',action-object-tmp))) into bindings)
                 (collect ``(,',ajax-p-tmp (when ,',action-object-tmp
                                             (action-ajax-p ,',action-object-tmp))) into bindings)
                 (finally (return `(let (,@macro-backup-bindings)
                                    (let* (,@macro-bindings)
                                      (declare (ignorable ,@macro-ignorables))
                                      `(let* (,,@bindings)
                                        (declare (ignorable ,@',ignorables))
                                        ,,@body)))))))))))

;;;; Parenscript

(eval-always

(deftag-macro <ucw:script (&attribute (compile-time-p nil) (toplevelp nil)
                                      &allow-other-attributes others
                                      &body body)
  "Insert a script tag compiling each expression in BODY with (js:js* ...).

When COMPILE-TIME-P is true the js:js* calls are made as part of the macroexpansion,
and the resulting strings are concatenated and emitted as a single string constant
at runtime.

Unless TOPLEVELP is true the body is wrapped in a JS closure, so it has its own
'toplevel' bindings that are separated from the global variables of the page
(though they are shared between the expressions in BODY).

If you want to define page-global variables then you must enable TOPLEVELP, but
then you should also be aware of how JS closures work. If unsure, simply avoid
installing event handlers from TOPLEVELP <ucw:script tags.

The most common pitfall is to think that in

(<ucw:script :toplevelp t
  `(let ((var value))
    (lambda ()
      body))
  `(let ((var value))
    (lambda ()
      body2)))

BODY has its own binding of VAR, but in fact VAR is compiled into a page-global
variable. The JS closure, that references VAR, will see the current value of
the page-global binding of VAR as opposed to a copy of it made at the time the
closure was created.

Please note that in either case VAR is shared between BODY and BODY2."

  (unless toplevelp
    (setf body (list ``((lambda ()
                          ,,@body)))))
  `(<:script :type "text/javascript"
             ,@others
             (<:as-is #\Newline "// <![CDATA[" #\Newline
                      ,(if compile-time-p
                           (iter (for expr in body)
                                 (collect (js:js* (eval expr)) :into result)
                                 (finally (return (apply #'concatenate 'string result))))
                           `(js:js* ,@body))
                      #\Newline "// ]]>" #\Newline)))
) ; eval-always

;;;; ** UCW Tags

(defun install-action-js (dom-node event-name action &key
                                   forms-to-submit forms-to-abandon
                                   progress-label)
  (setf forms-to-submit (ensure-list forms-to-submit))
  (setf forms-to-abandon (ensure-list forms-to-abandon))
  (multiple-value-bind (action-href uri)
      (action-href action :component (or (and (boundp '*current-component*)
                                              *current-component*)
                                         (context.window-component *context*)))
    (declare (ignore action-href))
    (<ucw:script :toplevelp t
                 `(ucw.event.install-event-handler ,dom-node ,event-name
                   (create :url ,(uri.path uri)
                           :session-id ,(session.id (context.session *context*))
                           :frame-id ,(frame.id (context.current-frame *context*))
                           :action-id ,(action-id action)
                           :ajaxp ,(to-js-boolean (action-ajax-p action))
                           ,@(when forms-to-submit
                                   `(:forms-to-submit (array ,@forms-to-submit)))
                           ,@(when forms-to-abandon
                                   `(:forms-to-abandon (array ,@forms-to-abandon)))
                           ,@(when progress-label
                                   (list :progress-label progress-label)))))))

(defun expand-link-tag (tag-name &key id action onclick skip-submit href other-attributes body
                                 progress-label)
  (assert (xor action (or onclick href)) () "You may only provide either an action or and onclick and/or a href parameter")
  (cond
    ((or onclick href) `(,tag-name :href (or ,href "#") :onclick ,onclick ,@other-attributes ,@body))
    (action (unless id
              (setf id `(js:gen-js-name-string :prefix "id")))
            (with-unique-names (id-value)
              `(let ((,id-value ,id))
                (,tag-name :href (action-href ,action)
                 :id ,id-value
                 ,@other-attributes
                 ,@body)
                (apply #'install-action-js ,id-value "onclick" ,action
                                           :progress-label ,progress-label
                                           (when (inside-a-form-p)
                                             (list (if ,skip-submit
                                                       :forms-to-abandon
                                                       :forms-to-submit)
                                                   (current-form-id))))
                ,id-value)))))

(deftag-macro <ucw:a (&attribute id action action-body href onclick skip-submit progress-label
                                 &allow-other-attributes others &body body)
  "Execute ACTION when the link is followed."
  (with-extracted-actions (others action)
    (expand-link-tag '<:a :id id :action (and action-provided-p action-object)
                     :skip-submit skip-submit :href href :onclick onclick :progress-label progress-label
                     :other-attributes others :body body)))

(deftag-macro <ucw:area (&attribute id action action-body href onclick skip-submit progress-label
                                    &allow-other-attributes others &body body)
  "Execute ACTION when the link is followed."
  (with-extracted-actions (others action)
    (expand-link-tag '<:area :id id :action (and action-provided-p action-object)
                     :skip-submit skip-submit :href href :onclick onclick :progress-label progress-label
                     :other-attributes others :body body)))

;;;; *** Form tags

(deftag-macro <ucw:simple-form (&attribute (id '(js:gen-js-name-string :prefix "ucw-form"))
                                            action
                                            &allow-other-attributes others
                                            &body body)
  "A Simple form tag, made for use with SIMPLE-SUBMIT. Does not require javascript."
  (with-unique-names (url query action-object)
    (rebinding (id)
      `(let* ((,action-object (register-action ()
                                (ucw.rerl.info "Executing action ~S" ',action)
                                (with-call/cc ,action)))
              (,url (compute-url ,action-object *current-component*)))
         (<:form :action (print-uri-to-string-sans-query ,url)
                 ,@others
                 (dolist (,query (uri.query ,url))
                   (if (string= ,+action-parameter-name+ (car ,query))
                       (<:input :type "hidden" :name ,+action-parameter-name+
                                :value (when ,action (cdr ,query))
                                :id (action-id ,action-object))
                       (<:input :type "hidden" :name (car ,query) :value (cdr ,query))))
                 ,@body)))))

(deftag-macro <ucw:simple-submit (&attribute action &allow-other-attributes others &body body)
  "Presents an button tag which executes ACTION when clicked."  
  `(<:button :type "submit"
             :name +action-parameter-name+
             :value (action-id (register-action ()
                                 (ucw.rerl.info "Executing action ~S" ',action)
                                 (with-call/cc ,action)))
             ,@others ,@body))

(defmacro with-tag-scripts ((id &key default-form-action focused) &body body)
  "Handles the :default and :focused uniformly for all tags where they apply."
  (let ((scripts ``((when ,,default-form-action
                      (<ucw:script `(ucw.form.set-default-action ,(current-form-id) ,,,id)))
                    (when ,,focused
                      (<ucw:script `(.focus ($ ,,,id)))))))
    `(rebinding (,id)
      `(progn
        ,,@body
        ,@,scripts))))

(deftag-macro <ucw:button (&attribute action action-body default focused
                                      progress-label forms-to-submit
                                      (id '(js:gen-js-name-string :prefix "_ucw_button"))
                                      &allow-other-attributes others &body body)
  "Presents an button tag which executes ACTION when clicked."
  (with-extracted-actions (others action)
    (with-tag-scripts (id :default-form-action default :focused focused)
      `(progn
        (<:button :id ,id ,@others ,@body)
        (install-action-js ,id "onclick" ,action-object
                           :forms-to-submit ,(or forms-to-submit
                                                 `(list (current-form-id)))
                           :progress-label ,progress-label)))))

(defun register-submit-callback (callback)
  (declare (type function callback))
  (push callback (submit-callbacks-of (current-form))))

(defun inside-a-form-p ()
  "Returns whether we are wrapped by an <ucw:form tag."
  (current-form))

(defun current-form ()
  (when (and (boundp '*current-form*)
             *current-form*)
    *current-form*))

(defun current-form-id ()
  (dom-id *current-form*))

(defcomponent html-form-dummy (html-form)
  ()
  (:documentation "This is used by <ucw:form when there's not any component representing a form."))

(deftag-macro <ucw:form (&attribute html-form wrapper-tag-name id
                                    action action-body onsubmit method
                                    dirty-p-checker (abandon-handler :ask-user)
                                    &allow-other-attributes others
                                    &body body)
  "Generic FORM tag replacement. When the form is submitted the
form ACTION will be eval'd, unless another action is specified
in a nested submit tag. If no ACTION is provided the form won't
be submittable by pressing enter in a form element.

  HTML-FORM's may have so called submit-callbacks. They will be called
whenever the form is submitted and before any actions are executed.

  It's possible to add ABANDON-HANDLER's, there are two UCW provided
handlers: :ask-user and :auto-submit. Otherwise it may be a parenscript
lambda that will be called when the form is dirty and it is about to be
abandoned. By default it's :ask-user, but forms that have no dirty
checkers are considered non-modified and therefore their abandon
handlers are not called.

  You may install your own DIRTY-P-CHECKER, it may be a parenscript
lambda or a mere true/false constant."
  (assert (or (and (not html-form) (not id)) (xor html-form id)) () "Can't define both :id and :html-form for <ucw:form")
  (unless id
    (setf id (js:gen-js-name-string :prefix "uform")))
  (unless html-form
    (setf html-form `(make-instance 'html-form-dummy :id ,id)))
  (with-extracted-actions (others action)
    (when onsubmit
      (error "<ucw:form does not support the :onsubmit attribute."))
    (with-unique-names (nestedp id-tmp url query submit-action submit-callback-id other-attributes)
      (rebinding (abandon-handler html-form wrapper-tag-name)
        `(let ((,url ,(when action-provided-p
                       `(when ,action-object
                         (compute-url ,action-object *current-component*))))
               (,other-attributes (list ,@others)))
          (let* ((,nestedp (or (and (boundp '*current-form*)
                                    (not (eq *current-form* ,html-form)))
                               (ajax-rendering-in-progress-p)))
                 (*current-form* ,html-form)
                 (,id-tmp (dom-id *current-form*)))
            (ucw.component.render.dribble "Rendering a <ucw:form tag while *current-component* is ~S" *current-component*)
            {with-xml-syntax
              <(if (or ,nestedp
                       ,wrapper-tag-name)
                   (or ,wrapper-tag-name
                       "div")
                   "form")
                 :action (when ,url
                           (print-uri-to-string-sans-query ,url))
                 :id ,id-tmp
                 :method ,method
                 :onsubmit (js:js-inline* `(return (ucw.form.default-onsubmit this)))
                 ;; (@ ,@others) can't work this way, because WITH-XML-SYNTAX happens at read-time
                 (@ ,other-attributes)
                 (if ,url
                     (dolist (,query (uri.query ,url))
                       (<:input :type "hidden" :name (car ,query) :value (cdr ,query)))
                     (progn
                       (<:input :type "hidden" :name ,+session-parameter-name+)
                       (<:input :type "hidden" :name ,+frame-parameter-name+)
                       (<:input :type "hidden" :name ,+action-parameter-name+)))
                 ;; render a first fake input on which mozilla will call onclick when the user hits enter
                 (<:input :type "image" :style "display: none")
                 ;; render an invocation id
                 (<:input :type "hidden" :name ,+action-invocation-parameter-name+)
                 ,@body
                 (let ((,submit-callback-id))
                   (awhen (submit-callbacks-of *current-form*)
                     (setf ,submit-callback-id (js:gen-js-name-string :prefix "sbmtclbk"))
                     ;; emit a callback that will call the registered submit-callbacks
                     (<:input :id ,submit-callback-id
                              :type "hidden"
                              :name (register-callback
                                     (let ((callback-trunks it))
                                       (lambda (value)
                                         (declare (ignore value))
                                         (ucw.rerl.actions.dribble "<ucw:form is now calling the following submit callbacks trunks: ~A" callback-trunks)
                                         (iter (for callback in callback-trunks)
                                               (funcall callback))
                                         (ucw.rerl.actions.dribble "<ucw:form finished calling the submit callbacks trunks")))
                                     :priority -100))
                     (ucw.rerl.actions.dribble "<ucw:form has cleared the registered submit callbacks of ~A" *current-form*)
                     (setf (submit-callbacks-of *current-form*) '()))
                   (<ucw:script
                    `(let ((form ($ ,,id-tmp)))
                      (ucw.form.register form)
                      ,(awhen ,submit-callback-id
                          `(ucw.field.register form ,,submit-callback-id))
                      ,,(awhen dirty-p-checker
                               ``(setf form.ucw-dirty-p ,',it))
                      ,(cond ((eq ,abandon-handler :ask-user)
                              `(setf form.ucw-approve-abandoning ucw.form.ask-user-abandon-handler))
                             ((eq ,abandon-handler :auto-submit)
                              (let ((,submit-action (register-ajax-action ()
                                                      ;; nop: we just have the callbacks called
                                                      )))
                                `(setf form.ucw-approve-abandoning
                                  (lambda ()
                                    (return (ucw.form.auto-submit-abandon-handler
                                             form
                                             (create
                                              :session-id ,(session.id (context.session *context*))
                                              :frame-id ,(frame.id (context.current-frame *context*))
                                              :action-id ,(action-id ,submit-action))))))))
                             (,abandon-handler
                              `(setf form.ucw-approve-abandoning ,,abandon-handler))))))
                 (values)>}))))))

(deftag-macro <ucw::simple-select (&attribute writer accessor on-change on-change-body
                                              on-change-progress-label
                                              name (id (js:gen-js-name-string :prefix "_sel"))
                                              &allow-other-attributes others
                                              &body body)
  "<:select with callback and on-change support. Use <:option to render the options."
  (with-extracted-actions (others on-change)
    (assert (not (and (or on-change on-change-body)
                      (member :onchange others)))
            (others)
            "Can't supply both :on-change and :onChange")
    (assert (or accessor writer)
            (others)
            "You need to supply either an accessor or a writer to <ucw:simple-select")
    (with-unique-names (id-value v)
      (let ((writer (or writer `(lambda (,v) (setf ,accessor ,v)))))
        `(let ((,id-value ,id))
          (<:select :name (register-callback
                           (lambda (,v)
                             (funcall ,writer ,v))
                           :id ,name)
                    :id ,id-value
                    ,@others
                    ,@body)
          (when ,on-change-id
            (install-action-js ,id-value "onchange" ,on-change-object
                               :forms-to-submit (current-form-id)
                               :progress-label ,on-change-progress-label)))))))

(deftag-macro <ucw::%select (&attribute multiple writer accessor (test '#'eql) (key '#'identity)
                                        on-change-progress-label
                                        on-change on-change-body name (id (js:gen-js-name-string :prefix "sel"))
                             &allow-other-attributes others
                             &body body)
  "The implementation of <ucw:select and tal tags with a ucw:accessor (or ucw:writer) attribute.

if multiple is non-nil the writer is a (possibly empty) list of
values, otherwise the single value is used."
  (with-extracted-actions (others on-change)
    (assert (not (and (or on-change on-change-body)
                      (member :onchange others)))
            (others)
            "Can't supply both :on-change and :onChange")
    (assert (or accessor writer)
            (others)
            "You need to supply either an accessor or a writer to <ucw:select")
    (with-unique-names (id-value v val values)
      (let ((writer (or writer `(lambda (,v) (setf ,accessor ,v)))))
        `(let ((%current-select-value ,accessor)
               (%current-select-test ,test)
               (%current-select-key ,key)
               (%select-table nil)
               (%multiple ,multiple)
               (,id-value ,id))
          (declare (ignorable %current-select-value %current-select-test %current-select-key
                    %select-table %multiple))
          (<:select :name (register-callback
                           (flet ((get-associated-value (v)
                                    (let ((v (assoc v %select-table :test #'string=)))
                                      (if v
                                          (cdr v)
                                          (error "Unknown option value: ~S." v)))))
                             (if %multiple
                                 (lambda (,v)
                                   (iterate
                                     (for ,val in (ensure-list ,v))
                                     (collect (get-associated-value ,val) into ,values)
                                     (finally (funcall ,writer ,values))))
                                 (lambda (,v) (funcall ,writer (get-associated-value ,v)))))
                           :id ,name)
                    :id ,id-value
                    ,@others
                    ,@(when multiple `(:multiple %multiple))
                    ,@body)
          (when ,on-change-id
            (install-action-js ,id-value "onchange" ,on-change-object
                               :forms-to-submit (current-form-id)
                               :progress-label ,on-change-progress-label)))))))

(deftag-macro <ucw:select (&allow-other-attributes others
                           &body body)
  `(<ucw::%select ,@others ,@body))

(deftag-macro <ucw::%option (&attribute value &allow-other-attributes others &body body)
  (with-unique-names (value-id)
    (rebinding (value)
      `(let ((,value-id (random-string 10)))
        (push (cons ,value-id ,value) %select-table)
        (<:option :value ,value-id
         ;;NB: we are applying key to both the option value being rendered,
         ;; as well as the selected value(s).
         ;;That was how the code worked previously, I don't know if it is desirable.
         ;;I think the alternative would be to apply the key to ",value" that is
         ;; the option being rendered, and remove the :key argument from find.

         ;;The logical operation we are trying to accomplish is
         ;;(mapcar #'add-selected-attribute
         ;;          (find-all %current-select-value(s)
         ;;                    (list-of-collected-<ucw::%option-calls)
         ;;                    :key %current-select-key))
                  :selected (when (find
                                   (funcall %current-select-key ,value) ;key applied to an option
                                   (if %multiple
                                       %current-select-value
                                       (list %current-select-value))
                                   :test %current-select-test
                                   :key %current-select-key)
                              T)
         ,@others ,@body)))))

(deftag-macro <ucw:option (&allow-other-attributes others &body body)
  "Replacement for the standard OPTION tag, must be used with
  <UCW:SELECT tag. Unlike \"regular\" OPTION tags the :value
  attribute can be any lisp object (printable or not)."
  `(<ucw::%option ,@others ,@body))

(deftag-macro <ucw:input (&attribute accessor action action-body reader writer name focused
                                     progress-label
                                     (id '(js:gen-js-name-string :prefix "_ucw_input"))
                                     (default nil)
                          &allow-other-attributes others)
  "Generic INPUT tag replacement.

If the ACCESSOR attribute is specified then it must be a PLACE
and it's value will be used to fill the input, when the form is
submitted it will be set to the new value.

If ACTION is specefied then when the form is submitted via this
input type=\"submit\" tag the form will be eval'd. when the
submit (or image) is clicked. DEFAULT means that the ACTION
provided for this input tag will be the default action of the
form when pressing enter in a form field. If more then one, then
the latest wins."
  (with-extracted-actions (others action)
    (when (and action action-body accessor)
      (error "Must specify only one of (or :ACTION :ACTION-BODY) and :ACCESSOR."))
    (when (and accessor (or reader writer))
      (error "Can not specify both :ACCESSOR and :READER or :WRITER."))
    (when (and (not (or action action-body)) default)
      (warn "Only those <ucw:input tags can be default that have an action"))
    (with-tag-scripts (id :default-form-action default :focused focused)
      (cond
        ((or reader accessor writer)
         (with-unique-names (v)
           (let ((type (getf others :type))
                 (value (gensym "INPUT-"))
                 (reader (or reader accessor))
                 (writer (or writer `(lambda (,v)
                                      (setf ,accessor ,v)))))
             `(let ((,value ,reader))
               (<:input :name (register-callback ,writer :id ,name)
                :id ,id
                :value ,value
                ,@(when (string= "checkbox" type)
                        `(:checked ,value))
                (@ ,@others))))))
        ((or action action-body)
         `(progn
           (<:input :id ,id :name ,name (@ ,@others))
           (install-action-js ,id "onclick" ,action-object
                              :forms-to-submit (current-form-id)
                              :progress-label ,progress-label)))
        (t
         (error "Must specify either :ACTION or :ACCESSOR (or use a regular <:input tag.)"))))))

(deftag-macro <ucw:textarea (&attribute id accessor reader writer name focused
                             &allow-other-attributes others
                             &body body)
  "TEXTAREA input tag which accepts the ACCESSOR attribute. The
  semantics of the ACCESSOR are the same as the accessor
  attribute of the <ucw:input tag."
  (let ((reader (or reader accessor))
        (writer (or writer (with-unique-names (v)
                             `(lambda (,v) (setf ,accessor ,v))))))
    (when (and focused (not id))
      (setf id '(js:gen-js-name-string :prefix "_ucw_textarea")))
    (remf-keywords others :id :name)
    (with-unique-names (read-value)
      (with-tag-scripts (id :focused focused)
        `(<:textarea :name (register-callback ,writer :id ,name)
          :id ,id
          (@ ,@others)
          (when-bind ,read-value ,reader
            (<:as-html ,read-value)))))))

;;;; *** Form tag shortcuts

(defmacro defform-input-tag-shortcut (tag-name input-type)
  `(deftag-macro ,tag-name (&allow-other-attributes others)
     `(<ucw:input :type ,,input-type ,@others)))

(defform-input-tag-shortcut <ucw:text "text")

(defform-input-tag-shortcut <ucw:password "password")

(defform-input-tag-shortcut <ucw:submit "submit")

;;;; *** Miscalenous form convience tags

(deftag-macro <ucw:integer-range-select (&attribute min max (step 1) &allow-other-attributes others &body body)
  "Tag for creating select/options pairs of sequential numbers."
  (if (every #'constantp (list min max step))
      `(<ucw:select ,@others
         ,@body
         ,@(iterate
            (for i from min to max by step)
            (collect `(<ucw:option :value ,i ,(princ-to-string i)))))
      `(<ucw:select ,@others
         ,@body
         (iterate
           (for i from ,min to ,max by ,step)
           (<ucw:option :value i (<:as-html i))))))

(deftag-macro <ucw:month-day-select (&attribute accessor &body body)
  `(<ucw:integer-range-select :min 1 :max 31 :accessor ,accessor ,@body))

(deftag-macro <ucw:month-select (&attribute accessor &allow-other-attributes others &body body)
  `(<ucw:select :accessor ,accessor
     ,@others ,@body
     (<ucw:option :value 1 "January")
     (<ucw:option :value 2 "February")
     (<ucw:option :value 3 "March")
     (<ucw:option :value 4 "April")
     (<ucw:option :value 5 "May")
     (<ucw:option :value 6 "June")
     (<ucw:option :value 7 "July")
     (<ucw:option :value 8 "August")
     (<ucw:option :value 9 "September")
     (<ucw:option :value 10 "October")
     (<ucw:option :value 11 "November")
     (<ucw:option :value 12 "December")))

(deftag <ucw:render-component (&attribute component)
  (emit-code `(ucw::render ,component)))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
