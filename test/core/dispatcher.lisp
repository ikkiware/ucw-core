(in-package :ucw-test)

(defsuite* (test/dispatcher :in test) ()
  (with-fixture ensure-test-application
    (run-child-tests)))

(defclass test-dispatcher (action-dispatcher)
  ())

(defparameter +dispatcher-test-answer+ "Test Dispatcher? Brilliant!")
(defparameter +dispatcher-test-path+ (strcat +test-application-url-prefix+ "dispatch/brilliant.ucw"))

(defmethod dispatch ((dispatcher test-dispatcher) app context)
  "The same usual test for a special case and/or move-on"
  (if (string= (query-path (context.request context)) +dispatcher-test-path+)
      (progn
        (out +dispatcher-test-answer+)
        t)
      (call-next-method)))

(defun make-test-dispatcher ()
  (make-instance 'test-dispatcher))

(defixture ensure-test-dispatcher
  (:setup
   (ensure-test-application :setup)
   (push (make-test-dispatcher) (application.dispatchers *test-application*)))
  (:teardown
   (setf (application.dispatchers *test-application*)
         (delete-if (rcurry #'typep 'test-dispatcher)
                    (application.dispatchers *test-application*)))
   (ensure-test-application :teardown)))

(defapptest test/dispatcher/answers ()
  (with-fixture ensure-test-dispatcher
    (is (string= +dispatcher-test-answer+ (web +dispatcher-test-path+))))
  (is (not (string= +dispatcher-test-answer+ (web +dispatcher-test-path+ :expected-status 404)))))


