(in-package :cl-user)

(defpackage #:it.bese.ucw.test
  (:nicknames :ucw-test)
  (:use :closer-common-lisp :ucw-core :stefil :iterate :arnesi :c2mop :yaclml)
  (:shadow #:parent #:test #:uri #:deftest)
  (:export #:test))

(in-package :ucw-test)



