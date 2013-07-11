;; See the file LICENCE for licence information.
(in-package :ucw)

;;;;;;;;;;;;;;;
;;; application

(defclass l10n-application-mixin ()
  ((default-locale
     :initform nil
     :initarg :default-locale
     :type list
     :accessor default-locale-of
     :documentation "Something that the cl-l10n locale function understands")
   (resource-package
    :initarg :resource-package
    :accessor resource-package-of
    :documentation "When bound cl-l10n's *resource-package* is bound to this value.")
   (accepted-locales
    :initform '()
    :initarg :accepted-locales
    :accessor accepted-locales-of
    :documentation "When not nil the user-requested locales will be filtered according to this list."))
  (:documentation "Application class which can handle l10n requests."))

(defmethod initialize-instance :after ((app l10n-application-mixin) &key)
  ;; explicitly call our customized setf's to resolve the locale name
  (awhen (default-locale-of app)
    (setf (default-locale-of app) it))
  (awhen (accepted-locales-of app)
    (setf (accepted-locales-of app) it)))

(defmethod (setf default-locale-of) :around (locale (app l10n-application-mixin))
  (typecase locale
    (locale (setf (default-locale-of app) (normalize-locale-list (list locale))))
    (list   (call-next-method (normalize-locale-list locale) app))
    (t      (with-resource-package (resource-package-of app)
              (setf (default-locale-of app) (normalize-locale-list (list (locale locale))))))))

(defmethod (setf accepted-locales-of) :around ((locales list) (app l10n-application-mixin))
  (with-resource-package (resource-package-of app)
    (call-next-method (mapcar #'locale locales) app)))


;;;;;;;;;;;
;;; session

(defclass l10n-session-mixin ()
  ((client-timezone :accessor client-timezone-of :initarg :client-timezone :initform *default-timezone*)))

(defmethod session-class list ((app l10n-application-mixin))
  'l10n-session-mixin)

(defmethod handle-action :around (action application (session l10n-session-mixin) frame)
  (let ((*client-timezone* (client-timezone-of session)))
    (call-next-method)))

(defun render-client-timezone-probe ()
  "Renders an input field with a callback that will set the CLIENT-TIMEZONE slot of the session when the form is submitted."
  (let ((id (js:gen-js-name-string)))
    (<:input :id id
             :type "hidden"
             :name (register-callback
                    (lambda (value)
                      (if (and value
                               (not (zerop (length value))))
                          (let ((local-time (parse-rfc3339-timestring value)))
                            (ucw.l10n.debug "Setting client timezone from ~A" local-time)
                            (setf (client-timezone-of (context.session *context*)) (timezone-of local-time)))
                          (progn
                            (ucw.l10n.warn "Unable to parse the client timezone: ~S" value)
                            (setf (client-timezone-of (context.session *context*)) +utc-zone+))))))
    (<ucw:script `(on-load
                   (setf (slot-value ($ ,id) 'value)
                         (dojo.date.to-rfc-3339 (new *date)))))))


;;;;;;;;;;;
;;; context

(defclass l10n-request-context-mixin (request-context)
  ((locale :initarg :locale))
  (:documentation "Request context for l10n apps. Contains one
  extra slot: the list of locales associated with this request.
  This is directly usable with the cl-l10n:with-locale macro."))

(defmethod request-context-class list ((app l10n-application-mixin))
  'l10n-request-context-mixin)

(define-shared-hashtable accept-language-cache :test #'equal :purge-interval-size +accept-language-cache-purge-size+)

(defmethod context.locale (context)
  (current-locale))

(defmethod context.locale ((context l10n-request-context-mixin))
  (if (slot-boundp context 'locale)
      (slot-value context 'locale)
      (progn
        (ucw.l10n.dribble "Trying to get locale for the context from the Accept-Language header")
        (let* ((locale nil)
               (request (context.request context))
               (app (context.application context))
               (default-locale (default-locale-of app))
               (accept-language-value (get-header request "Accept-Language")))
          (setf locale (or (when accept-language-value
                             (ensure-accept-language-cache-value accept-language-value
                               (ucw.l10n.info "Cache miss for Accept-Language header value '~S'"
                                              accept-language-value)
                               (normalize-locale-list (process-accept-language app request))))
                           default-locale))
          (dolist (el default-locale)
            (unless (member el locale)
              ;; make sure default-locale's elements are always in the locale list
              (setf locale (append locale (list el)))))
          (ucw.l10n.debug "Setting context.locale to ~S" locale)
          (setf (context.locale context) locale)))))

(defmethod (setf context.locale) (value (context l10n-request-context-mixin))
  (setf (slot-value context 'locale) value))

(defmethod make-request-context :around ((app l10n-application-mixin) (request request) (response response))
  (if (slot-boundp app 'resource-package)
      (with-resource-package (resource-package-of app)
        (call-next-method))
      (call-next-method)))

(defmethod service :around ((app l10n-application-mixin) (context l10n-request-context-mixin))
  (flet ((body ()
           (aif (context.locale context)
                (with-locale it
                  (ucw.l10n.debug "Binding *locale* to ~S" it)
                  (call-next-method))
                (progn
                  (ucw.l10n.debug "Context has no locale, leaving alone *locale*")
                  (call-next-method)))))
    (if (slot-boundp app 'resource-package)
        (with-resource-package (resource-package-of app)
          (body))
        (body))))

(defgeneric process-accept-language (application request)
  (:method ((app l10n-application-mixin) (request request))
           (awhen (get-header request "Accept-Language")
             (when (> (length it) +maximum-accept-language-value-length+)
               (ucw.l10n.warn "Refusing to parse Accept-Language header value, its length is ~S" (length it))
               (return-from process-accept-language nil))
             (let ((langs (parse-accept-language-header it)))
               (ucw.l10n.debug "Parsed language header ~S, app default locale is ~S, app accepted locales are ~S"
                               langs (default-locale-of app) (accepted-locales-of app))
               (iter (with accepted-locales = (accepted-locales-of app))
                     (for (lang weigth) in langs)
                     (for locale = (locale lang :errorp nil))
                     (ucw.l10n.debug "Looked up locale ~S from ~S" locale lang)
                     (unless locale
                       (next-iteration))
                     (when (and (or (not accepted-locales)
                                    (member locale accepted-locales))
                                (not (member locale result :key #'car)))
                       (collect (cons locale weigth) :into result))
                     (finally
                      (setf result (mapcar #'car (sort result #'> :key #'cdr)))
                      (awhen (default-locale-of app)
                        (setf result (nconc result (copy-list it))))
                      (ucw.l10n.debug "The final sorted locale list is ~S" result)
                      (return result)))))))

(defclass l10n-application (standard-application l10n-application-mixin)
  ()
  (:documentation "See L10N-APPLICATION-MIXIN for details."))





