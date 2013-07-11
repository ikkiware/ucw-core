;; See the file LICENCE for licence information.
(in-package :ucw)

(eval-always
  (shadowing-import '(cl-l10n:locale cl-l10n:locale-name) :ucw)
  (use-package :cl-l10n :ucw)
  (use-package :cl-l10n :ucw-user)
  (setup-readtable))

(defvar *client-timezone*)

(defun reload-ucw-resources ()
  "This method reloads the UCW resources needed by some UCW services. Invoke
   after your :lang package has been set up."
  (flet ((system-relative-pathname (system path)
           (merge-pathnames path (asdf:component-pathname
                                  (asdf:find-system system)))))
    (let ((resource-dir (system-relative-pathname
                         :ucw (make-pathname :directory (list :relative "src" "l10n" "resources")
                                             :name :wild
                                             :type "lisp"))))
      (ucw.l10n.debug "Looking for UCW resources in dir ~S" resource-dir)
      (dolist (resource-file (directory resource-dir))
        (ucw.l10n.info "Loading UCW resource file ~S" resource-file)
        (load resource-file)))))

(defmacro define-js-resources (locale &body resources)
  "Wraps defresources and registers the resources being defined
as js resources, which means that they are going to be sent down to
the client side in a cacheable js file."
  `(progn
    (register-js-resources
     ,@(iter (for (name . body) in resources)
             (when (> (length body) 1)
               (error "How should we send functional resources to js? ~S has arguments..." name))
             (collect `',name)))
    (defresources ,locale ,@resources)))

(defclass l10n-tal-generator (file-system-generator)
  ()
  (:documentation "TAL File system generator which maps template
  names to files while considering the locale of the request
  context."))

(defmethod yaclml:template-truename ((generator l10n-tal-generator) (name pathname))
  ;; first see if there's a locale specific version
  (when-bind locales (context.locale *context*)
    (ucw.l10n.debug "Looking for tal directory with locales ~A" locales)
    (iter (for locale in locales)
          (for file = (make-pathname :defaults name
                                     :directory (list :relative (locale-name locale))))
          (ucw.l10n.debug "Trying tal file ~A" file)
          (awhen (call-next-method generator file)
            (ucw.l10n.debug "Found locale specific tal directory ~A" it)
            (return-from yaclml:template-truename it))))
  ;; no locale for this context, use the default
  (ucw.l10n.debug "No locale specific tal directory found for locale file ~A" name)
  (call-next-method))

(deftag-macro <ucw:localized (&attribute (warn-if-missing t) args &body forms)
  "Just like <:as-html but calls localize on the elements before rendering and
if a resource cannot be found then wraps it in a <:span with a css class.
Capitalize the first letter of the looked up string if the key also started with
a capital letter."
  `(progn
    ,@(mapcar (lambda (form)
                (if (stringp form)
                    `(multiple-value-bind (str foundp) (lookup-resource ,form ,args
                                                                        :warn-if-missing ,warn-if-missing
                                                                        :fallback-to-name t)
                       (if foundp
                           (<:as-html ,(if (upper-case-p (elt form 0))
                                           `(capitalize-first-letter str)
                                           `str))
                           (<:span :class #.+missing-resource-css-class+
                                   (<:as-html str))))
                    (rebinding (form)
                      `(multiple-value-bind (str foundp) (lookup-resource ,form ,args
                                                                          :warn-if-missing ,warn-if-missing
                                                                          :fallback-to-name t)
                         (if foundp
                             (<:as-html (if (upper-case-p (elt ,form 0))
                                            (capitalize-first-letter str)
                                            str))
                             (<:span :class #.+missing-resource-css-class+
                                     (<:as-html str)))))))
              forms)))

(defun dojo-locale-name-for (locale)
  (substitute #\- #\_
              (string-downcase
               (locale-name locale))))

;; TODO cl-l10n should meet local-time
;; TODO move into cl-l10n
(defun ucw.lang:timestamp (local-time &key
                           (relative-mode nil)
                           (display-day-of-week nil display-day-of-week-provided?))
  "DWIMish timestamp printer. RELATIVE-MODE requests printing human friendly dates based on (NOW)."
  (setf local-time (local-time:adjust-local-time local-time (set :timezone ucw:*client-timezone*)))
  (bind ((*print-pretty* nil)
         (*print-circle* nil)
         (cl-l10n::*time-zone* nil)
         (now (local-time:now)))
    (with-output-to-string (stream)
      (with-decoded-local-time (:year year1 :month month1 :day day1 :day-of-week day-of-week1) now
        (with-decoded-local-time (:year year2 :month month2 :day day2) local-time
          (bind ((year-distance (abs (- year1 year2)))
                 (month-distance (abs (- month1 month2)))
                 (day-distance (- day2 day1))
                 (day-of-week-already-encoded? nil)
                 (day-of-week-should-be-encoded? (or display-day-of-week
                                                     (and relative-mode
                                                          (zerop year-distance)
                                                          (zerop month-distance)
                                                          (< (abs day-distance) 7))))
                 (format-string/date (if (and relative-mode
                                              (zerop year-distance))
                                         (if (zerop month-distance)
                                             (cond
                                               ((<= -1 day-distance 1)
                                                (setf day-of-week-already-encoded? t)
                                                (setf day-of-week-should-be-encoded? nil)
                                                (case day-distance
                                                  (0  #"today")
                                                  (-1 #"yesterday")
                                                  (1  #"tomorrow")))
                                               (t
                                                ;; if we are within the current week
                                                ;; then print only the name of the day.
                                                (if (and (< (abs day-distance) 7)
                                                         (<= 1 ; but sunday can be a potential source of confusion
                                                             (+ day-of-week1
                                                                day-distance)
                                                             6))
                                                    (progn
                                                      (setf day-of-week-already-encoded? t)
                                                      "%A")
                                                    (progn
                                                      (setf day-of-week-should-be-encoded? nil)
                                                      "%b. %e."))))
                                             "%b. %e.")
                                         "%Y. %b. %e."))
                 (format-string (concatenate 'string
                                             format-string/date
                                             (if (or day-of-week-already-encoded?
                                                     (not day-of-week-should-be-encoded?)
                                                     (and display-day-of-week-provided?
                                                          (not display-day-of-week)))
                                                 ""
                                                 " %A")
                                             " %H:%M:%S")))
            (cl-l10n::print-time-string format-string stream
                                        (local-time:universal-time
                                         (local-time:adjust-local-time! local-time
                                           (set :timezone local-time:*default-timezone*)))
                                        (current-locale))))))))

(defun ucw.lang:timestamp<> (local-time &key (relative-mode t))
  (<:span :class "timestamp"
          (<:as-html (ucw.lang:timestamp local-time :relative-mode relative-mode))))

(def-tag-handler <ucw::file-modification-time (tag)
  (let ((filename (getf (cdar tag) '<ucw::file)))
    `(ucw.lang:file-last-modification-timestamp<>  ,filename)))

(defun render-url-to-file-with-modification-timestamp (filename directory
                                                       &key (url-prefix "static/")
                                                       icon content)
  (let* ((absolute-filename (merge-pathnames filename directory)))
    (<:span :class "file-url"
      (<:a :href (concatenate 'string url-prefix (namestring filename))
           (<:as-html content)
           (when icon
             (<:img :class "file-icon" :src icon)))
      (<:as-html " (")
      (ucw.lang:file-last-modification-timestamp<> absolute-filename)
      (<:as-html ")"))))

(def-tag-handler <ucw::url-to-file-with-modification-timestamp (tag)
  (let* ((filename (pathname (getf (cdar tag) '<ucw::file)))
         (directory (pathname (getf (cdar tag) '<ucw::directory))))
    `(render-url-to-file-with-modification-timestamp
      ,filename ,directory
      :url-prefix ,(or (getf (cdar tag) '<ucw::url-prefix)
                      "static/")
      :icon ,(getf (cdar tag) '<ucw::icon)
      :content ,(or (getf (cdar tag) '<ucw::content)
                    (file-namestring filename)))))

;;;; * i18n-parenscript-dispatcher

(defclass i18n-parenscript-dispatcher (dispatcher url-matcher parenscript-handler)
  ((locale-to-js-map :initform (make-hash-table :test 'equal) :accessor locale-to-js-map-of)
   (purge-cache-size :initform 10000 :initarg :purge-cache-size :accessor purge-cache-size-of)
   (last-purged-at :initform (get-universal-time) :accessor last-purged-at-of)
   (purge-interval :initform #.(* 60 60 24) :initarg :purge-interval :accessor purge-interval-of))
  (:default-initargs
     :priority +parenscript-dispatcher-default-priority+
     :url-string +i18n-parenscript-dispatcher-url+)
  (:documentation "This handler is used to serve a
compiled javascript string from *js-resource-registry*."))

(defmethod parenscript-handler-timestamp ((self i18n-parenscript-dispatcher))
  *js-resource-registry-last-modified-at*)

(defmethod parenscript-handler-compile ((self i18n-parenscript-dispatcher))
  (let* ((forms `(ucw.i18n.define
                  ,@(iter (for (name) in-hashtable *js-resource-registry*)
                          (collect (string-downcase (string name)))
                          (collect (localize name)))))
         (result (js:js* forms)))
    (ucw.rerl.dispatcher.info "~S is compiling for key ~S" self (i18n-parenscript-dispatcher-cache-key))
    (ucw.rerl.dispatcher.debug "~S at key ~S compiled ~S" self (i18n-parenscript-dispatcher-cache-key) result)
    result))

(defun i18n-parenscript-dispatcher-cache-key ()
  (mapcar #'locale-name (ensure-list *locale*)))

(defmethod cached-javascript ((self i18n-parenscript-dispatcher))
  (let* ((key (i18n-parenscript-dispatcher-cache-key))
         (result (gethash key (locale-to-js-map-of self))))
    (ucw.rerl.dispatcher.debug "~S ~A cached js with key ~S"
                               self (if result "found" "missed") key)
    result))

(defmethod (setf cached-javascript) (value (self i18n-parenscript-dispatcher))
  (bind ((now (get-universal-time))
         (cache (locale-to-js-map-of self)))
    (when (or (> (hash-table-count cache)
                 (purge-cache-size-of self))
              (> now
                 (+ (last-purged-at-of self)
                    (purge-interval-of self))))
      (ucw.rerl.dispatcher.info "~S is purging its cache" self)
      (setf (last-purged-at-of self) now)
      (clrhash cache))
    (let ((key (i18n-parenscript-dispatcher-cache-key)))
      (ucw.rerl.dispatcher.debug "~S is storing cached js with key ~S" self key)
      (setf (gethash key cache) value))))


;; let's redefine the old ucw stubs, so that already expanded #""'s will work, too
(defun ucw-lookup-resource (name args)
  (lookup-resource name args))
(defun ucw-capitalize-first-letter (str)
  (capitalize-first-letter str))

;; and change the functions to be the cl-l10n versions for later expansions of the reader macro
(setf *sharpquote-resource-lookup-function* 'lookup-resource)
(setf *sharpquote-capitalize-first-letter-function* 'capitalize-first-letter)
