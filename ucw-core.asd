;;; -*- lisp -*-

;;;; ASDF system definition file for UCW
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.ucw.system)
    (defpackage :it.bese.ucw.system
      (:nicknames #:ucw.system)
      (:export #:*load-as-production-p*)
      (:use :common-lisp :asdf))))

(in-package :it.bese.ucw.system)


;;; Export the variables in the ucw.system package, so that between
;;; (asdf:find-system :ucw) and (asdf:oos 'asdf:load-op :ucw) users
;;; get the chance to set these variables when loading UCW
;;; programmatically. For more details on the variables themselves see
;;; src/vars.lisp

(macrolet ((def (&rest names)
               `(progn
                 ,@(loop for name in names
                         collect `(defvar ,name)
                         collect `(export ',name)))))
  (def
    *ucw-swank-port*
    *ucw-backend-type*
    *ucw-backend-host*
    *ucw-backend-port*
    *ucw-server-class*
    *ucw-applications-directory*
    *ucw-systems*
    *ucw-applications*
    *ucw-log-root-directory*
    *ucw-log-level*
    *ucw-compile-time-log-level*))

(defparameter *load-as-production-p* t
  "When T, load the UCW lisp files so that it will be used in a production system.
This means that debug-only blocks are skipped and various variables are initialized accordingly.")

(defclass ucw-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component ucw-source-file))
  (let ((*features* *features*))
    (unless *load-as-production-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defclass ucw-system (system)
  ((test-system :initform :ucw.core.test :initarg :test-system :accessor test-system-of)))

(defmacro defsystem* (name &body args)
  `(defsystem ,name :default-component-class ucw-source-file
    ,@args))

(defsystem* :ucw-core
  :description "Core features of UnCommon Web"
  :long-description "Contains the base features essential for a useful
Read Eval Render Loop (RERL)."
  :author "Marco Baringer <mb@bese.it>"
  :licence "BSD (sans advertising clause)"
  :version "0.9"
  :class ucw-system
  :test-system :ucw-core.test
  :components
  ((:module :src
    :components ((:file "core-package")
                 (:file "helpers" :depends-on ("core-package" "vars"))
                 (:file "loggers" :depends-on ("core-package" "vars"))
                 (:file "vars" :depends-on ("core-package"))
                 (:file "control" :depends-on (:backend :rerl))
                 (:module :backend
                  :components ((:file "accept-headers"))
                  :depends-on ("core-package" "loggers" :rerl))
                 (:module :rerl
                  :components ((:file "protocol")
                               (:file "rerl-variables")
                               (:file "rerl-utils" :depends-on ("protocol" "rerl-variables"))
                               (:file "conditions" :depends-on ("protocol"))
                               (:file "backtracking" :depends-on ("basic-classes"))
                               (:file "request-loop-error" :depends-on ("conditions" "rerl-utils" "basic-action"))
                               (:file "basic-classes" :depends-on ("protocol"
                                                                   "rerl-variables"))
                               (:file "basic-action" :depends-on ("protocol"
                                                                  "standard-session-frame"
                                                                  "basic-classes"))
                               (:file "basic-application" :depends-on ("rerl-utils"
                                                                       "basic-classes"))
                               (:module :standard-component
                                        :components ((:file "standard-component" :depends-on ("standard-component-class"))
                                                     (:file "control-flow" :depends-on ("standard-component"))
                                                     (:file "standard-component-class")
                                                     (:file "transactions" :depends-on ("standard-component")))
                                        :depends-on ("backtracking"
                                                     "rerl-utils"
                                                     "request-loop-error"
                                                     "basic-application"
                                                     "standard-session-frame"
                                                     "basic-action"
                                                     "basic-classes"))
                               (:file "basic-dispatchers" :depends-on ("request-loop-error"
                                                                       "basic-application"
                                                                       "basic-action"))
                               (:file "standard-request-context" :depends-on ("rerl-utils"
                                                                              "basic-classes"
                                                                              :standard-component))
                               (:file "standard-server" :depends-on ("rerl-utils"
                                                                     "request-loop-error"
                                                                     "basic-classes"))
                               (:file "basic-session" :depends-on ("rerl-utils"
                                                                   "basic-classes"
                                                                   "standard-session-frame"))
                               (:file "standard-session-frame" :depends-on ("rerl-utils"
                                                                            "backtracking"
                                                                            "basic-classes")))
                  :depends-on ("core-package" "loggers" "helpers" "vars"))
		 (:module :core-components
			  :components ((:file "window"))
			  :depends-on (:rerl)))))
  :properties ((version "0.9"))
  :depends-on (:arnesi :swank :iterate :yaclml :local-time
               :usocket :rfc2109 :net-telent-date :cl-fad
               :trivial-garbage :bordeaux-threads :closer-mop))

;; Backends

(defsystem* :ucw.httpd
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "message-queue")
                             (:file "basic-backend" :depends-on ("common"))
                             (:file "httpd" :depends-on ("message-queue" "basic-backend" "common")))))
  :depends-on (:ucw-core :rfc2388-binary :puri :cl-ppcre))

(defsystem* :ucw.mod-lisp
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "mod-lisp"))))
  :depends-on (:ucw-core :ucw.httpd :iolib.sockets))

(defsystem* :ucw.iolib
  :components ((:module :src
                :pathname "src/backend/"
                :components ((:file "common")
                             (:file "basic-backend" :depends-on ("common"))
                             (:file "iolib" :depends-on ("basic-backend" "common")))))
  :depends-on (:ucw-core :rfc2388-binary :puri :iolib.sockets :cl-ppcre))

(defsystem* :ucw-core.test
  :components ((:module :test
                :components
                ((:file "package")
                 (:file "test-environment" :depends-on ("package"))
                 (:module "core"
                          :depends-on ("test-environment")
                          :serial t
                          :components ((:file "server")
                                       (:file "application")
                                       (:file "dispatcher")
                                       (:file "entry-point")
                                       (:file "component")
                                       (:file "action")
                                       (:file "callbacks")))
                 (:file "stress" :depends-on ("core")))))
  :depends-on (:ucw-core :cxml :stefil :drakma :arnesi :iterate))

