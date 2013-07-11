;; -*- lisp -*-

(in-package #:ucw-standard)

;;;; ** Range View

(defclass paged-list ()
  ((offset :initarg :offset
           :accessor paged-list.offset
           :initform 0
           :backtrack t
           :documentation "Which of the pages we're currently looking at.")
   (pages :reader paged-list.pages :initform '())
   (page-size :accessor paged-list.page-size :initform 20 :initarg :page-size))
  (:documentation
   "Component for showing the user a set of data one \"page\" at a time.

The data set is presented one \"page\" at a time with links to
the the first, previous, next and last page. Each page shows
at most PAGE-SIZE elements of the data. The data is passed to
the paged-list at instance creation time via the :DATA initarg.

The generic function RENDER-PAGED-LIST-ITEM is used to render
each item of DATA.

In order to change the rendering of the single elements of a
range view developer's should create a sub class of PAGED-LIST
and define their RENDER-PAGED-LIST-ITEM methods on that.")
  (:metaclass standard-component-class))

(defun partition-into-pages (data page-size)
  (iterate
    (with pages = '())
    (with current-page = '())
    (for index upfrom 1)
    (for ele in data)
    (push ele current-page)
    (when (zerop (mod index page-size))
      (push (nreverse current-page) pages)
      (setf current-page '()))
    (finally (when current-page
               (push (nreverse current-page) pages)))
    (finally (return (nreverse pages)))))

(defmethod shared-initialize :after ((range paged-list) slot-names
                                     &key data (page-size (paged-list.page-size range)) &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value range 'pages)
	(partition-into-pages data page-size)))

(defmethod paged-list.current-page ((range paged-list))
  (nth (paged-list.offset range) (paged-list.pages range)))

;; (defmethod template-component-environment nconc ((range paged-list))
;;   (let ((current-page (paged-list.current-page range))
;;         current-page-number)
;;     (make-standard-tal-environment
;;      `((items . ,(mapcar (lambda (item-cons)
;;                            (tal-env 'index (car item-cons)
;;                                     'item (cdr item-cons)))
;;                          current-page))
;;        (pages . ,(loop 
;;                       for page-number upfrom 1
;;                       for w in (paged-list.pages range)
;;                       when (eq w current-page)
;;                         do (setf current-page-number page-number)
;;                       collect (tal-env 'num page-number 'selected (eq current-page w))))
;;        (current-page-number . ,current-page-number)
;;        (nextp . ,(paged-list.have-next-p range))
;;        (previousp . ,(paged-list.have-previous-p range))
;;        (num-pages . ,(length (paged-list.pages range)))))))

(defmethod paged-list.current-page-items ((range paged-list))
  (mapcar #'cdr (paged-list.current-page range)))

(defmethod paged-list.have-previous-p ((view paged-list))
  "Returns true if VIEW has a page before the current one."
  (and (paged-list.pages view)
       (not (zerop (paged-list.offset view)))))

(defmethod paged-list.have-next-p ((view paged-list))
  "Returns true if VIEW has a page after the current one."
  (with-slots (offset pages)
      view
    (and pages (< offset (1- (length pages))))))

(defmethod paged-list.page-count ((list paged-list))
  (length (paged-list.pages list)))

(defgeneric render-paged-list-item (paged-list item)
  (:documentation "Render a single element of a paged-list.")
  (:method ((paged-list paged-list) (item t))
    "Standard implementation of RENDER-PAGED-LIST-ITEM. Simply
applies ITEM to princ (via <:as-html)."
    (declare (ignore paged-list))
    (<:as-html item)))

(defmethod/cc scroll-start ((range paged-list))
  (setf (paged-list.offset range) 0))

(defmethod/cc scroll-end ((range paged-list))
  (setf (paged-list.offset range) (1- (length (paged-list.pages range)))))
  
(defmethod/cc scroll-forward ((view paged-list) &optional (n 1))
  (with-slots (offset pages)
      view
    (incf offset n)
    (when (<= (length pages) offset)
      (scroll-end view))))

(defmethod/cc scroll-backward ((range paged-list) &optional (n 1))
  (with-slots (offset)
      range
    (decf offset n)
    (when (minusp offset)
      (setf offset 0))))

(defmethod/cc scroll-to-page ((range paged-list) page-number)
  (setf (paged-list.offset range) page-number))

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
