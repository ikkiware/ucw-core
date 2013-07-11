;; -*- lisp -*-

(in-package :ucw-standard)

;;;; ** HTML-elements

(defvar *xml-id-counter* 0)

(defclass html-element-mixin ()
  ((css-class :initarg :css-class
              :initform nil
              :accessor html-element.css-class
              :type (or null string list))
   (dom-id :initarg :dom-id
           :initform nil
           :accessor html-element.dom-id
           :type (or null string))
   (css-style :initarg :css-style
              :initform nil
              :accessor html-element.css-style
              :type (or null string)))
  (:documentation "An HTML element.

HTML elements control aspects that are relevant to almost all tags.

They provide a place to store the class, id, and style of the
component. The specific render methods of the components themselves
must pass these values to whatever code is used to render the actual
tag."))

(defclass html-block-element-mixin (html-element-mixin)
  ()
  (:documentation "A component which should be wrapped in a <div>."))

(defclass html-inline-element-mixin (html-element-mixin)
  ()
  (:documentation "A component which should be wrapped in a <span>"))

(macrolet ((widget-render-helper (widget div-or-span)
	      `(let ((css-class (html-element.css-class ,widget))
		     (dom-id (html-element.dom-id ,widget))
		     (css-style (html-element.css-style ,widget)))
		 (if (or css-class dom-id css-style)
		     (,div-or-span :class css-class
				   :id dom-id
				   :style css-style
				   (call-next-method))
		     (call-next-method)))))
  (defmethod render :wrap-around ((widget html-block-element-mixin))
    "Wrap component in a <div> tag."
    (widget-render-helper widget <:div))

  (defmethod render :wrap-around ((widget html-inline-element-mixin))
    "Wrap component in a <span> tag."
    (widget-render-helper widget <:span)))

(defun unique-dom-id (&key  (prefix "_ucw_"))
  "Generates a unique DOM id"
  (format nil "~A~A" prefix (incf *xml-id-counter*)))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; Copyright (c) 2008 Clinton Ebadi
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
