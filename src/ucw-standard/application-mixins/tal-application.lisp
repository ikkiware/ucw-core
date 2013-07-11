(in-package #:ucw-standard)

(defclass tal-application-mixin (application)
  ((tal-generator :accessor application.tal-generator
                  :initarg :tal-generator
                  :documentation "A tal-generator object used to
lookup and compile tal pages for template-components.")))