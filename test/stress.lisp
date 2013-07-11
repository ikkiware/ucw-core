(in-package :ucw-test)

(defsuite* (test/stress :in test) ()
  (with-fixture ensure-test-application-with-action-dispatcher
    (test/stress/define-stress-component)
    (with-ucw-logger-level +warn+
      (without-test-progress-printing
        (run-child-tests)))))

(defparameter +stress-test-action-entry-point+ "stress-test-action.ucw")
(defparameter *print-stress-test-output* nil)
(defparameter *stress-test-quit-request* nil)
(defparameter *stress-test-max-call-depth* 100)

(deftest test/stress/define-stress-component ()
  (with-fixture ensure-test-application
    (finishes
      (with-test-compiler-environment
        (test/action/define-test-actions-component)
        (eval `(progn
                (defcomponent stress-test-actions-component (test-component)
                  ((action-invocation-counter
                    :initform 0
                    :accessor action-invocation-counter-of)
                   (counting-action-href :accessor counting-action-href-of)
                   (self-calling-action-href :accessor self-calling-action-href-of)
                   (answering-action-href :accessor answering-action-href-of))
                  (:entry-point +stress-test-action-entry-point+ (:application *test-application*))
                  (:render (self)
                   (setf (counting-action-href-of self) (action-href
                                                         (register-action (:make-new-frame nil)
                                                           (incf (action-invocation-counter-of self)))
                                                         :component self))
                   (setf (self-calling-action-href-of self) (action-href
                                                             (register-action (:with-call/cc t)
                                                               (is (= 42 (call-component self (make-instance (class-of self))))))
                                                             :component self))
                   (setf (answering-action-href-of self) (action-href
                                                          (register-action ()
                                                            (answer-component self 42))
                                                          :component self))
                   (render-component-as-plist self)))
                (defmethod update-url ((self stress-test-actions-component) uri)
                  (append-path-to-uri uri +stress-test-action-entry-point+)
                  uri)))))))

(defun read-stress-test-state (href &key
                                    (parse-result-as :sexp)
                                    (invocation-id (make-invocation-id)))
  (web (strcat href "&" +action-invocation-parameter-name+ "=" invocation-id)
       :parse-result-as parse-result-as))

(defun stress-output (format &rest args)
  (when *print-stress-test-output*
    (apply #'format *debug-io* format args)))

(def-action-test (test/stress/counter-action-triggering-thread :auto-call nil) ()
  (flet ((read-state (previous-state action-href-name &rest args)
           (apply #'read-stress-test-state (getf previous-state action-href-name) args)))
    (let ((session-id))
      (unwind-protect
           (labels ((recurse (start-state call-depth)
                      (assert (equal (getf start-state 'session-id)
                                     session-id))
                      (iter (with current-state = start-state)
                            (with action-counter = 0)
                            (until *stress-test-quit-request*)
                            (if (> (- (random *stress-test-max-call-depth*)
                                      call-depth)
                                   0)
                                (progn
                                  (stress-output "Calling while call-depth is ~A~%" call-depth)
                                  (recurse (read-state  current-state 'self-calling-action-href)
                                           (1+ call-depth)))
                                (if (zerop (random 3))
                                    (progn
                                      (stress-output "Answering while call-depth is ~A~%" call-depth)
                                      (read-state current-state 'answering-action-href)
                                      (return-from recurse))
                                    (progn
                                      (stress-output "Testing while call-depth is ~A~%" call-depth)
                                      (setf current-state (read-state current-state 'counting-action-href))
                                      (is (same-session-p start-state current-state))
                                      (if (zerop (getf current-state 'action-invocation-counter))
                                          (progn
                                            (stress-output "Reached a too old frame~%")
                                            (is (not (same-frame-p start-state current-state)))
                                            (setf start-state current-state)
                                            (setf action-counter 0))
                                          (progn
                                            (is (same-frame-p start-state current-state))
                                            (incf action-counter)))
                                      (is (eql action-counter (getf current-state 'action-invocation-counter)))))))))
             (let ((start-state (read-action-test-state :entry-point +stress-test-action-entry-point+)))
               (setf session-id (getf start-state 'session-id))
               (recurse start-state 0)))
        (when session-id
          (break "This is your last chance for inspecting stuff")
          (delete-test-session session-id))))))

(deftest (test/stress/raw-response-throughput :auto-call nil) ()
  (with-fixture ensure-raw-test-application
    (setf (raw-data-of *test-application*) (string-to-octets (random-string (* 1024 32)) :utf-8))
    (break "You may stress test ~A with something like:~%~
            httperf --rate 1000 --num-conn 10000 --port 9010 --server localhost --uri /raw-test-app/"
           (strcat +test-server-base-url+
                   (subseq +raw-test-application-url-prefix+ 1))))
  (values))

