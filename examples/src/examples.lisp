;;;; -*- lisp -*-

(in-package #:ucw-user)

(defvar *example-server* (make-instance 'standard-server))

;;;; The definiton of the example application

(defclass example-application (static-roots-application-mixin
			       tal-application-mixin
			       standard-application)
  ())

(defvar *wwwroot*
  (merge-pathnames #P"examples/wwwroot/"
		   (asdf:component-pathname (asdf:find-system :ucw))))

(defvar *example-application*
  (make-instance
   'example-application
   :url-prefix "/"
   :tal-generator (make-instance 'yaclml:file-system-generator
				 :cachep t
				 :root-directories (list (merge-pathnames #P"tal/" *wwwroot*)))
   :debug-on-error t
   :static-roots (list (cons "static/" (merge-pathnames #P"static/" *wwwroot*)))))

;;;; define the window component

(defcomponent example-window (standard-window-component)
  ()
  (:default-initargs
    :title "UCW Examples"
    :stylesheet (list "static/ucw.css" "static/examples.css")
    :body (make-instance 'tabbed-pane
			 :current-component-key 'example-welcome
			 :contents
                         `((example-welcome . ,(make-instance 'example-welcome))
			   (multiplication-table . ,(make-instance 'multiplication-table))
                           (counter . ,(make-instance 'counter))
                           (sum . ,(make-instance 'sum))
			   (wiki . ,(make-instance 'wiki-viewer :page-name "WelcomePage"))
                           ;(file-upload-example .   ,(make-instance 'file-upload-example))
                           ;(timeout-cache-example . ,(make-instance 'timeout-cache-example :timeout 10))
                           ;(hits-cache-example .    ,(make-instance 'hits-cache-example :timeout 5))
			   )))
  (:documentation "The main window component for the example application.

This component contains the list of all the available components
and simply wraps the rendering of the current component with the
navigation bar."))

(defentry-point "^(index.ucw|)$" (:application *example-application*
                                  :class regexp-dispatcher)
    ()
  (call 'example-window))


(defentry-point "mul.ucw" (:application *example-application*
			   :class simple-dispatcher
			   :with-call/cc nil
			   :action-options (:class 'action))
    ()
  (mul-table-example))

(defentry-point "mul-direct.ucw" (:application *example-application*
	  		          :class minimal-dispatcher
				  :with-call/cc nil
				  :action-options (:class 'action))
    ()
  (direct-mul-table-example))

(defcomponent example-welcome (html-block-element-mixin)
  ()
  (:documentation "The first page seen by the example app. This
component does nothing other than render a litte introductory
text.")
  (:render ()
    (<:h1 "UCW Examples")
    (<:p (<:as-html "Click on a link to try a demo."))))

(defcomponent multiplication-table (html-block-element-mixin)
  ()
  (:documentation "Just show a few links to mul.ucw / mul-direct.ucw")
  (:render ()
     (<:h1 "Multiplication Table Examples")
     (<:ul
      (<:li (<:p (<:a :href "mul.ucw" "YACLML Multiplication table")))
      (<:li (<:p (<:a :href "direct-mul.ucw"
		      "Direct Stream Writing Multiplication table"))))))

;;;; multiplication table

(defun mul-table-example ()
  (yaclml:with-yaclml-stream (html-stream (context.response *context*))
    (with-request-params (n) (context.request *context*)
      (let ((n (if n
                   (or (parse-integer n :junk-allowed t)
                       0)
                   0)))
        (<:html
         (<:head (<:title "Multiplication table"))
         (<:body
	  (<:a :href "index.ucw" "Go Back to Main Demo")
          (<:h1 "Multiplication table upto " (<:ah n))
          (<:form :action "" :method "GET"
                  (<:p "N: " (<:input :type "text" :name "n") (<:input :type "submit" :value "Calculate")))
          (<:table
           (<:tr
            (<:th)
            (loop
               for i from 1 to n
               do (<:th (<:ah i))))
           (loop
              for i from 1 to n
              do (<:tr
                  (<:th (<:ah i))
                  (loop
                     for j from 1 to n
                     do (<:td (<:ah (* i j)))))))))))))

(defun direct-mul-table-example ()
  ;; just like the above example but write directly to the client
  ;; stream. you can usually tell the difference if N is large.
  (send-headers (context.response *context*))
  (with-request-params (n) (context.request *context*)
    (let ((n (if n
                 (or (parse-integer n :junk-allowed t)
                     0)
                 0)))
      (flet ((send-string (&rest strings)
               (let ((network-stream (ucw::network-stream (context.response *context*))))
                 (dolist (string strings)
                   (write-sequence (string-to-octets (if (stringp string)
                                                         string
                                                         (princ-to-string string))
                                                     :us-ascii)
                                   network-stream))
                 (write-sequence +CR-LF+ network-stream))))
        ;; we can't use YACLML here since the respons'se network stream is an (unsigned-byte 8) stream.
        (send-string "<html>")
        (send-string "<head><title>Multiplication table</title></head>")
        (send-string "<body>")
	(send-string "<a href=\"index.ucw\">Go Back to Main Demo</a>")
        (send-string "<h1>Multiplication table upto " n "</h1>")
        (send-string "<form action=\"\">")
        (send-string "<p>N: <input type=\"text\" name=\"n\"><input type=\"submit\" value=\"Calculate\"></p> ")
        (send-string "</form>")
        (send-string "<table>")
        (send-string "<tr>")
        (send-string "<th></th>")
        (loop
           for i from 1 to n
           do (send-string "<th>" i "</th>"))
        (send-string "</tr>")
        (loop
           for i from 1 to n
           do (send-string "<tr>")
           do (send-string "<th>" i "</th>")
           do (loop
                 for j from 1 to n
                 do (send-string "<td>" (* i j) "</td>"))
           do (send-string "</tr>"))
        (send-string "</table>")))))

(defun start-example-server (&key (backend :httpd) (port 8000))
  (if (server.started *example-server*)
      (error "Server already started")
      (setf (server.backend *example-server*)
	    (make-backend backend :port port)))
  (register-application *example-server* *example-application*)
  (startup-server *example-server*))

(defun stop-example-server ()
  (when (server.started *example-server*)
    (shutdown-server *example-server*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; Copyright (c) 2009 Drew Crampsie <drewc@tech.coop>
;;; Copyright (c) 2009 Clinton Ebadi <clinton@unknownlamer.org>
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
