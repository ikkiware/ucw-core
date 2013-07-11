;; -*- lisp -*-

(in-package #:ucw-standard)

;;;; ** TAL Template

(defclass tal-component (component)
  ((template-name :accessor tal-component.template-name
                  :initarg :template-name
                  :initform nil))
  (:documentation "Component which is rendered via a TAL template."))

(defgeneric tal-component-environment (component)
  (:documentation "Create the TAL environment for rendering COMPONENT's template.

Methods defined on this generic function must return a TAL
environment: a list of TAL binding sets (see the documentation
for YACLML:MAKE-STANDARD-TAL-ENVIRONMENT for details on TAL
environments.)")
  (:method-combination nconc))

(defmethod tal-component-environment nconc ((component tal-component))
  "Create the basic TAL environment.

Binds the symbol ucw:component to the component object itself,
also puts the object COMPONENT on the environment (after the
binding of ucw:component) so that slots are, by default,
visable."
  (make-standard-tal-environment `((component . ,component)) component))

(defmethod render :around ((component tal-component))
  "Render a template based component.

The name of the template is the value returned by the generic function
TAL-COMPONENT.TEMPLATE-NAME, the template will be rendered
in the environment returned by the generic function
TAL-COMPONENT-ENVIRONMENT."
  (aif (tal-component.template-name component)
       (render-template *context*
                        it
                        (list* `((next-method-of-render . ,#'call-next-method))
                               (tal-component-environment component)))
       (call-next-method)))

(defcomponent simple-tal-component (tal-component)
  ((environment :initarg :environment :initform nil)))

(defmethod tal-component-environment nconc ((component simple-tal-component))
  (copy-list (slot-value component 'environment)))


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
