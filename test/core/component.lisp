(in-package :ucw-test)

(defsuite* (test/component :in test) ()
  (with-fixture ensure-test-application
    (run-child-tests)))

(defparameter +component-entry-point+ "test-component.ucw")
(defparameter +component-entry-point-1+ "simple-entry-call.ucw")
(defparameter +component-entry-point-2+ "simple-entry-call-2.ucw")

(deftest test/component/define (&key (default-backtrack #'identity))
  (with-fixture ensure-test-application
    (finishes
      (with-test-compiler-environment
        (eval `(defcomponent test-component ()
                ((message
                  :initarg :message
                  :initform "Component?"
                  :accessor message)
                 (backtracked-slot
                  :initarg :backtracked-slot
                  :initform :initial
                  :accessor backtracked-slot-of
                  :backtrack t)
                 (not-backtracked-slot
                  :initarg :not-backtracked-slot
                  :initform :initial
                  :accessor not-backtracked-slot-of
                  :backtrack nil))
                (:default-backtrack ,default-backtrack)
                (:entry-point +component-entry-point+ (:application *test-application*))
                (:render (self)
                 (out "Test ~A Brilliant!" (message self)))
                (:action)))))))

(defixture ensure-component-test-entry-points
  (:setup
   (ensure-test-application :setup)
   (test/component/define)
   (defentry-point +component-entry-point-1+ (:application *test-application*)
       ()
     (call 'test-component))
   (defentry-point +component-entry-point-2+ (:application *test-application*)
       ((message "Message?"))
     (call 'test-component :message message)))

  (:teardown
   (setf (application.dispatchers *test-application*)
         (delete-if (lambda (el)
                      (and (typep el 'url-matcher)
                           (member (url-string el) (list +component-entry-point+
                                                         +component-entry-point-1+
                                                         +component-entry-point-2+)
                                   :test #'string=)))
                    (application.dispatchers *test-application*)))
   (ensure-test-application :teardown)))

(deftest test/component/remote-args ()
  (with-fixture ensure-component-test-entry-points
    (is (string= "Test Component? Brilliant!"
                 (web (uri +component-entry-point-1+))))
    (is (string= "Test Message? Brilliant!"
                 (web (uri +component-entry-point-2+))))
    (is (string= "Test URL MESSAGE? Brilliant!"
                 (web (uri +component-entry-point-2+ "message" "URL MESSAGE?"))))))
  






