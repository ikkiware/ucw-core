(in-package :ucw-test)

(enable-bracket-syntax)

(in-suite test/action)

(defparameter +test-ajax-action-entry-point+ "test-ajax-action.ucw")

(deftest test/action/define-test-ajax-actions-component (&key (default-backtrack nil))
  (with-fixture ensure-test-application
    (finishes
      (with-test-compiler-environment
        (test/action/define-test-actions-component)
        (eval `(defcomponent test-ajax-actions-component (test-actions-component)
                ()
                (:entry-point +test-ajax-action-entry-point+ (:application *test-application*))
                (:default-backtrack ,default-backtrack)
                (:default-initargs
                   :action-maker
                    (let ((times-run 0))
                      (lambda (self)
                        (let ((action
                               (register-ajax-action ()
                                 (incf times-run)
                                 (setf (action-invocation-counter-of self) times-run)
                                 (incf (backtracked-slot-of self))
                                 (incf (not-backtracked-slot-of self))
                                 (incf (slot-for-default-backtrack-test-of self))
                                 (render-component-as-xml self))))
                          (is (not (action-make-new-frame-p action)))
                          action))))))))))

(defixture ensure-test-application-with-ajax-action-dispatcher
  (:setup
   (ensure-test-application-with-action-dispatcher :setup)
   (unless (find-if (rcurry #'typep 'ajax-action-dispatcher)
                    (application.dispatchers *test-application*))
     (register-dispatcher *test-application* (make-instance 'ajax-action-dispatcher))))
  (:teardown
   (setf (application.dispatchers *test-application*)
         (delete-if (lambda (el)
                      (typep el 'ajax-action-dispatcher))
                    (application.dispatchers *test-application*)))
   (ensure-test-application-with-action-dispatcher :teardown)))

(defmacro def-ajax-action-test (name args &body body)
  `(def-action-test ,name ,args
    (labels ((read-state (&rest args)
               (apply 'read-action-test-state
                      :entry-point +test-ajax-action-entry-point+
                      args))
             (read-ajax-answer (&rest args)
               (apply #'read-action-test-state
                      :entry-point +test-ajax-action-entry-point+
                      :parse-result-as :xmls
                      args))
             (component-slot (xmls slot-name &key as-integer)
               (let* ((nodes (cddr (third xmls)))
                      (result (third (find slot-name nodes :key #'first :test #'string=))))
                 (if as-integer
                     (parse-integer result :junk-allowed t)
                     result)))
             (same-session-p (state-1 state-2)
               (string= (getf state-1 'session-id)
                        (component-slot state-2 "session-id")))
             (same-frame-p (state-1 state-2)
               (string= (getf state-1 'frame-id)
                        (component-slot state-2 "frame-id"))))
      (declare (ignorable #'read-ajax-answer #'read-state
                          #'component-slot #'same-session-p #'same-frame-p))
      ,@body)))

(def-ajax-action-test test/action/remote-ajax-action (&key (default-backtrack nil))
  (with-fixture ensure-test-application-with-ajax-action-dispatcher
    (test/action/define-test-ajax-actions-component :default-backtrack default-backtrack)
    (let ((session-id-to-delete))
      (unwind-protect
           (let ((start (read-state)))
             (setf session-id-to-delete (getf start 'session-id))

             ;; Check the starting state
             (is (eql 0 (getf start 'action-invocation-counter)))

             ;; Call our first ajax action
             (let ((ajax-result (read-ajax-answer :previous-state start)))
               ;; (break "ajax result is: ~A" ajax-result)
               (is (string= (first ajax-result) "answer"))
               (is (= (length ajax-result) 4))
               (is (same-session-p start ajax-result))
               (is (same-frame-p start ajax-result))
               (is (= (component-slot ajax-result "action-invocation-counter"
                                      :as-integer t)
                      1))))

        ;; Get rid of our session
        (when session-id-to-delete
          (delete-test-session session-id-to-delete))))))