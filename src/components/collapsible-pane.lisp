;; -*- lisp -*-

(in-package :it.bese.ucw)

(def (constant e :test #'string=) +default-collapsible-collapsed-image-url+ "static/ucw/images/collapsible-off.png")
(def (constant e :test #'string=) +default-collapsible-expanded-image-url+ "static/ucw/images/collapsible-on.png")

;;;; ** Collapsible pane component

(defcomponent collapsible-pane (widget-component-with-body)
  ((switch :initform nil :initarg :switch :accessor switch-of)
   (collapsedp :initform t :initarg :collapsed :accessor collapsedp)
   (client-side :initform nil :initarg :client-side-p :accessor client-side-p))
  (:default-initargs
    :css-class "ucw-collapsible"
    :dom-id (js:gen-js-name-string :prefix "_collaps"))
  (:documentation "Component for good."))

(flet ((setup-body-parent (self)
         ;; break the parent relation of the body component when collapsed, so the ajax/dirty component
         ;; renderings will properly detect that this component is not visible currently
         (awhen (body-of self)
           (if (and (collapsedp self)
                    (not (client-side-p self)))
               (setf (parent it) nil)
               (setf (parent it) self)))))

  (defmethod initialize-instance :after ((self collapsible-pane) &key)
    (setup-body-parent self))
  
  (defmethod (setf body-of) :after (value (self collapsible-pane))
    (setup-body-parent self))

  (defmethod (setf collapsedp) :around (value (self collapsible-pane))
    (let ((old-value (collapsedp self)))
      (when (xor old-value value)
        (call-next-method)
        (setup-body-parent self)
        (mark-dirty self)))))

(defun render-standard-collapsible-pane-switch (self
                                                &rest args
                                                &key
                                                (action-class 'ajax-action)
                                                title
                                                (collapsed-title title)
                                                (expanded-title title)
                                                (escape-title t)
                                                (collapsed-image +default-collapsible-collapsed-image-url+)
                                                (expanded-image +default-collapsible-expanded-image-url+))
  (if (client-side-p self)
      (apply 'render-standard-client-side-collapsible-switch self args)
      (<:div :class (list "switch" (if (collapsedp self)
                                       "collapsed"
                                       "expanded"))
             (<ucw:a :action (register-action (:class action-class)
                               (setf (collapsedp self) (not (collapsedp self))))
                     (<:img :src (if (collapsedp self)
                                     collapsed-image
                                     expanded-image))
                     (if (collapsedp self)
                         (when collapsed-title
                           (if escape-title
                               (<:as-html collapsed-title)
                               (<:as-is collapsed-title)))
                         (when expanded-title
                           (if escape-title
                               (<:as-html expanded-title)
                               (<:as-is expanded-title))))))))

(defun render-standard-client-side-collapsible-switch (collapsedp
                                                       &key
                                                       title
                                                       (escape-title t)
                                                       (collapsed-image "static/ucw/images/collapsible-off.png")
                                                       (expanded-image "static/ucw/images/collapsible-on.png"))
  ;; ucw.widget.collapsible-pane.toggle depends on this node layout
  (<:div :class "switch"
         (<ucw:a :onclick (js:js-inline* `(progn
                                           (ucw.widget.collapsible-pane.toggle this ,collapsed-image ,expanded-image)
                                           (return false)))
                 (<:img :src (if collapsedp
                                 collapsed-image
                                 expanded-image))
                 (if escape-title
                     (<:as-html title)
                     (<:as-is title)))))

(defmacro within-client-side-collapsible-pane ((collapsedp
                                                &key title
                                                     (collapsed-image "static/ucw/images/collapsible-off.png")
                                                     (expanded-image "static/ucw/images/collapsible-on.png"))
                                               &body body)
  (rebinding (collapsedp)
    `(progn
      (render-standard-client-side-collapsible-switch ,collapsedp
       :title ,title
       :collapsed-image ,collapsed-image
       :expanded-image ,expanded-image)
      (<:div :class "collapsible-body"
             :style (when ,collapsedp
                      "display: none")
       ,@body))))

(defmethod render ((self collapsible-pane))
  (render-switch self)
  (<:div :class "collapsible-body"
         :style (when (and (client-side-p self)
                           (collapsedp self))
                  "display: none")
         (when (or (client-side-p self)
                   (not (collapsedp self)))
           (awhen (body-of self)
             (render it)))))

(defmethod render-switch ((self collapsible-pane))
  (if (client-side-p self)
      (progn
        (assert (not (switch-of self)) () "Client side collapsible-pane does not support a custom switch")
        (render-standard-collapsible-pane-switch self))
      (let ((switch (switch-of self)))
        (etypecase switch
          (function (funcall switch self))
          (string (render-standard-collapsible-pane-switch self :title switch))
          (null (render-standard-collapsible-pane-switch self))
          (component (render switch))))))

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
