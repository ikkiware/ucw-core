;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Simple Window

(defcomponent window-component ()
  ((content-type :accessor window-component.content-type
                 :initarg :content-type
                 :initform nil ; default is text/html with charset from current application
                 :documentation "The Content-Type header for the
                 http response (also used in the meta tag)")))

(defmethod window-component.content-type :around ((window window-component))
  "Either use slot value, or compute content-type from current application charset."
  (or (call-next-method)
      (setf (window-component.content-type window)
            (format nil "text/html~@[; charset=~A~]"
                    (application.charset (context.application *context*))))))

(defmethod render :before ((window window-component))
  (setf (get-header (context.response *context*) "Content-Type")
        (window-component.content-type window)))

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

 (:SRC url) - writes <script src=\"${app-url-prefix}URL\"></script> tag.
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
    (let* ((app (context.application *context*))
           (url-prefix (application.url-prefix app)))
      (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
      (awhen (window-component.title window)
        (<:title (if (functionp it)
                     (funcall it window)
                     (<:as-html it))))
      (awhen (window-component.icon window)
        (<:link :rel "icon"
                :type "image/x-icon"
                :href (concatenate 'string url-prefix it)))
      (dolist (stylesheet (effective-window-stylesheets window))
        (<:link :rel "stylesheet"
                :href stylesheet
                :type "text/css")))))

(defgeneric render-html-body (window)
  (:method :around ((window basic-window-features-mixin))
           (<:body
            (render-window-scripts window)
            (call-next-method))))

(defgeneric render-window-scripts (window)
  (:method ((window basic-window-features-mixin))
           (let* ((app (context.application *context*))
                  (url-prefix (application.url-prefix app)))
             (dolist* ((type value) (window-component.javascript window))
               (ecase type
                 (:src
                  (<:script :type "text/javascript"
                            :src (concatenate 'string url-prefix value)
                            ;; most browsers (firefox, safari and ie at least) really,
                            ;; really, really don't like empty script tags. The "" forces
                            ;; yaclml to generate a seperate closing tag.
                            ""))
                 ;; TODO clean up these names
                 (:js
                  (<ucw:script :toplevelp t
                               (if (functionp value)
                                   (funcall value)
                                   value)))
                 (:script
                  (<:script :type "text/javascript"
                            (<:as-is ~% "// <![CDATA[" ~%
                                     value
                                     ~% "// ]]>" ~%))))))))

(defcomponent basic-window-component (basic-window-features-mixin window-component)
  ()
  (:documentation "A convenience class for writing window components."))

(defcomponent dojo-window-component-mixin ()
  ((dojo-debug
    :type boolean
    :accessor dojo-debug-p
    :initarg :dojo-debug)
   (dojo-debug-at-all-costs
    :type boolean
    :accessor dojo-debug-at-all-costs-p
    :initarg :dojo-debug-at-all-costs)))

#+nil(defmethod render-html-head ((self dojo-window-component-mixin))
  (call-next-method)
  (<:link :rel "stylesheet" :href "dijit/dijit.css" :type "text/css"))

(defmethod dojo-debug-p :around ((self dojo-window-component-mixin))
  (if (slot-boundp self 'dojo-debug)
      (call-next-method)
      (debug-on-error (context.application *context*))))

(defmethod dojo-debug-at-all-costs-p :around ((self dojo-window-component-mixin))
  (if (slot-boundp self 'dojo-debug-at-all-costs)
      (call-next-method)
      (let ((app (context.application *context*)))
        (and (dojo-debug-p self)
             (and (typep app 'standard-application)
                  (string= (javascript-log-level app)
                           "debug"))))))

(defun dojo-locale-name-for (locale)
  (declare (ignore locale))
  (warn "dojo-locale-name-for should have been redefined by the cl-l10n integration"))

;; TODO propagate debug level to the dojo loggers.
(defmethod initialize-instance :around ((self dojo-window-component-mixin) &key)
  ;; we use an :around to delay this code as late as possible, because the dojo scripts
  ;; must be the first thing in the document.
  (call-next-method)
  (let* ((app (context.application *context*))
         (url-prefix (application.url-prefix app)))
    (setf (window-component.javascript self)
          (append (list
                   (list :js (lambda ()
                               (let ((locale (context.locale *context*)))
                                 (when (consp locale)
                                   (setf locale (first locale)))
                                 `(setf dj-config
                                   (create
                                    ,@(when locale
                                        `(:locale ,(dojo-locale-name-for locale)))
                                    ,@(if (dojo-debug-p self)
                                          `(:is-debug true
                                            :debug-container-id "dojoDebug"
                                            ,@(when (dojo-debug-at-all-costs-p self)
                                               `(:debug-at-all-costs true)))
                                          `(:is-debug false))
                                    :base-loader-uri ,(strcat url-prefix "static/dojo/"))))))
                   '(:src "static/dojo/dojo.js")
                   #+nil`(:script ,(js:js*
                               `(progn
                                 ;; dojo.registerModulePath("dijit", "../dijit"); is not needed if dijit and dojo are sibling directories
                                 (dojo.require "dijit.util.parser"))))
                   '(:src #.(map-to-dynamic-ucw-url "js/functional.js"))
                   '(:src #.(map-to-dynamic-ucw-url "js/per-application.js")))
                  (when (dojo-debug-p self)
                    `((:script ,(js:js*
                                 `(dojo.require "dojo.debug.console")))))
                  (window-component.javascript self)))))

(defmethod render-window-scripts :after ((self dojo-window-component-mixin))
  (when (dojo-debug-at-all-costs-p self)
    (<ucw:script :compile-time-p t
     `(dojo.hostenv.write-includes))))

(defcomponent standard-window-component (dojo-window-component-mixin basic-window-component)
  ((initial-polling-delay
    :type (or null integer)
    :initarg :initial-polling-delay
    :accessor initial-polling-delay-of
    :initform nil
    :documentation "Delay before the first polling connection after a full page load in usecs."))
  (:default-initargs
      ;; An example to return fully XML docs. But then don't expect your pages to work out of the box... :)
      ;;:content-type (format nil "text/xml~@[; charset=~A~]"
      ;;                      (application.charset (context.application *context*)))
      ;;:content-prologue (format nil "<?xml version=\"1.0\" encoding=\"~A\"?>"
      ;;                          (application.charset (context.application *context*)))
      :html-tag-attributes (list "xmlns"      #.+xhtml-namespace-uri+
                                 "xmlns:dojo" #.+dojo-namespace-uri+))
  (:documentation "Window component that ensures a proper environment for all the UCW features.
This window will load dojo and in general it will mess around with the browser environment. If
you don't want that, use BASIC-WINDOW-COMPONENT."))

;;; set up some UCW specific scripts
(defmethod render-window-scripts :after ((self standard-window-component))
  (<ucw:script :compile-time-p t
    `(dojo.event.connect self "onbeforeunload" ucw.default-unload-event-handler))
  (let ((session (context.session *context*)))
    (<ucw:script :toplevelp t
      `(progn
        (setf ucw.session-id ,(session.id session))
        (setf ucw.frame-id ,(frame.id (session.current-frame session))))))
  (awhen (initial-polling-delay-of self)
    (<ucw:script
     `(on-load
       (ucw.io.polling.start ,it)))))

(defcomponent standard-window-component-with-body (standard-window-component
                                                   component-body-mixin)
  ()
  (:documentation "Just like STANDARD-WINDOW-COMPONENT but it has a BODY slot and
a render method that will render its body."))

(defmethod render-html-body ((self standard-window-component-with-body))
  (render (body-of self)))

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
