;;; -*- lisp -*-

;;;; ASDF system definition file for UCW
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.ucw.system)
    (defpackage :it.bese.ucw.system
      (:nicknames #:ucw.system)
      (:export #:*load-as-production-p*)
      (:use :common-lisp :asdf))))

(in-package :it.bese.ucw.system)

;;; We need defsystem* from the ucw-core.asd
(asdf:oos 'asdf:load-op :ucw-core)

(defsystem* :ucw
  :description "UncommonWeb : Standard Components"
  :long-description "Containts a simple, standard component library."
  :author "Marco Baringer <mb@bese.it>"
  :maintainer "Drew Crampsie <drewc@tech.coop>"
  :licence "BSD (sans advertising clause)"
  :version "0.9"
  :class ucw-system
  :components
  ((:module :src
    :components 
    ((:module :ucw-standard
      :components ((:file "standard-package")
		   (:file "standard-action" :depends-on ("standard-package"))
		   (:file "standard-dispatchers" :depends-on ("standard-package"))
		   (:file "standard-tags" :depends-on ("standard-action"))
		   (:file "standard-components" :depends-on ("standard-tags"))
		   (:file "file-serve" :depends-on ("standard-package"))
		   (:module :application-mixins
		    :components ((:file "cookie-session-application")
				 (:file "secure-application")
				 (:file "static-roots-application")
				 (:file "tal-application")
				 (:file "transactional-application"))
		    :depends-on ("standard-package" "standard-dispatchers"
				 "components" "yaclml"))
		   (:module :components
		    :components ((:file "html-element")
				 (:file "cached")
				 (:file "container"
					:depends-on ("html-element"))
				 (:file "error")
				 (:file "option-dialog")
				 (:file "paged-list")
				 (:file "redirect")
				 (:file "tal")
				 (:file "task")
				 (:file "user-login"))
		    :depends-on ("standard-components"))
		   (:module :yaclml
		    :components ((:file "tal"))
		    :depends-on ("standard-tags")))))))
  :properties ((version "0.9"))
  :depends-on (:ucw-core :cl-ppcre :closer-mop))


(defsystem* :ucw.manual-examples
  :description "UncommonWeb : Examples"
  :long-description "The example code from the manual"
  :author "Marco Baringer <mb@bese.it>"
  :maintainer "Drew Crampsie <drewc@tech.coop>"
  :licence "BSD (sans advertising clause)"
  :version "0.9"
  :class ucw-system
  :components
  ((:module :doc
    :components 
    ((:file "example-code"))))
  :properties ((version "0.9"))
  :depends-on (:ucw))

(defsystem* :ucw.examples
  :license "BSD (sans advertising clause)"
  :version "0.9"
  :components
  ((:module :examples
	    :components ((:module :src
			  :components ((:file "examples")
				       (:file "counter" :depends-on ("examples"))
				       ;(:file "cache" :depends-on ("examples"))
				       ;(:file "forms" :depends-on ("examples"))
				       (:file "sum" :depends-on ("examples"))
				       ;(:file "shared-counter")
				       (:file "wiki" :depends-on ("examples")))))))
  :depends-on (:ucw :ucw.httpd))


(defsystem* :ucw-ps
  :description "UncommonWeb: Parenscript functionality"
  :maintainer "Erick López <erick@ikki.ws>"
  :licence "BSD 2 Clause (http://opensource.org/comment/1294)"
  :version "0.5"
  :class ucw-system
  :components
  ((:module :ucwps
    :components 
    ((:file "ucw-ps-package")
     (:file "ucw-ps" :depends-on ("ucw-ps-package")))))
  :properties ((version "0.5"))
  :depends-on (:ucw-core :ucw :arnesi :parenscript))
