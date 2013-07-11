(in-package :ucw-test)

(defsuite* (test/action :in test) ()
  (with-fixture ensure-test-application-with-action-dispatcher
    (run-child-tests)
    (test/action/remote-action :default-backtrack nil)
    (test/action/remote-action :default-backtrack t)
    (test/action/remote-action :default-backtrack #'identity)))

(defparameter +test-action-entry-point+ "test-action.ucw")
(defparameter +url-rewrite-entry-point+ "url-counter.ucw")
(defparameter +url-rewrite-parameter-name+ "counter-value-in-url")

(deftest test/action/define-test-actions-component (&key (default-backtrack nil))
  (with-fixture ensure-test-application
    (finishes
      (with-test-compiler-environment
        (test/component/define)
        (eval `(defcomponent test-actions-component (test-component)
                ((action-invocation-counter
                  :initform 0
                  :accessor action-invocation-counter-of
                  :backtrack nil)
                 (slot-for-default-backtrack-test
                  :initform 0
                  :accessor slot-for-default-backtrack-test-of)
                 (action-maker
                  :initarg :action-maker
                  :accessor action-maker-of)
                 (action-id :accessor action-id-of)
                 (invocation-isolated-action-id :accessor invocation-isolated-action-id-of))
                (:entry-point +test-action-entry-point+ (:application *test-application*))
                (:default-backtrack ,default-backtrack)
                (:default-initargs
                   :backtracked-slot 0
                   :not-backtracked-slot 0
                   :action-maker (let ((times-run 0))
                                   (lambda (self)
                                     (make-action
                                      (lambda ()
                                        (incf times-run)
                                        (setf (action-invocation-counter-of self) times-run)
                                        (incf (backtracked-slot-of self))
                                        (incf (not-backtracked-slot-of self))
                                        (incf (slot-for-default-backtrack-test-of self)))
                                      :class 'basic-action))))
                (:render (self)
                  (setf (action-id-of self) (register-action-in-frame
                                             (context.current-frame *context*)
                                             (funcall (action-maker-of self) self)))
                  (setf (invocation-isolated-action-id-of self)
                   (let ((action (register-action (:class 'action-with-isolation-support
                                                   :invocation-isolated t)
                                   (incf (action-invocation-counter-of self)))))
                     (is (action-isolated-p action))
                     ))
                  (render-component-as-plist self))))))))

(defixture ensure-test-application-with-action-dispatcher
  (:setup
   (ensure-test-application :setup)
   (unless (find-if (rcurry #'typep 'action-dispatcher)
                    (application.dispatchers *test-application*))
     (register-dispatcher *test-application* (make-instance 'action-dispatcher))))
  (:teardown
   (setf (application.dispatchers *test-application*)
         (delete-if (lambda (el)
                      (typep el 'action-dispatcher))
                    (application.dispatchers *test-application*)))
   (ensure-test-application :teardown)))

(defun read-action-test-state (&key (entry-point +test-action-entry-point+)
                                    (parse-result-as :sexp)
                                    previous-state (action-id-property-name 'action-id)
                                    (invocation-id (make-invocation-id)))
  (let ((params (list)))
    (when previous-state
      (assert (getf previous-state action-id-property-name))
      (setf params (list +session-parameter-name+ (getf previous-state 'session-id)
                         +frame-parameter-name+   (getf previous-state 'frame-id)
                         +action-parameter-name+  (getf previous-state action-id-property-name))))
    (web (apply 'uri entry-point params) :parse-result-as parse-result-as)))

(defmacro def-action-test (name args &body body)
  `(deftest ,name ,args
    (with-fixture ensure-test-application-with-action-dispatcher
      (flet ((delete-test-session (session-id)
               (let ((sessions-deleted 0))
                 (iterate-sessions-with-lock-held *test-application*
                                                  (lambda (session)
                                                    (when (string= (session.id session) session-id)
                                                      (mark-session-expired session)
                                                      (incf sessions-deleted))))
                 (is (eql sessions-deleted 1))))
             (read-state (&rest args)
               (apply 'read-action-test-state args))
             (same-session-p (state-1 state-2)
               (string= (getf state-1 'session-id)
                        (getf state-2 'session-id)))
             (same-frame-p (state-1 state-2)
               (string= (getf state-1 'frame-id)
                        (getf state-2 'frame-id))))
        (declare (ignorable #'delete-test-session #'read-state
                            #'same-session-p #'same-frame-p))
        ,@body))))

;; We call this test directly from the suite with 3 different args for DEFAULT-BACKTRACK.
;; So, specify :AUTO-CALL NIL to tell the suite to leave it alone in RUN-CHILD-TESTS.
(def-action-test (test/action/remote-action :auto-call nil) (&key (default-backtrack nil))
  (test/action/define-test-actions-component :default-backtrack default-backtrack)
  (let ((session-id-to-delete))
    (unwind-protect
         (let ((start (read-state)))
           (setf session-id-to-delete (getf start 'session-id))

           ;; Check the starting state
           (is (eql 0 (getf start 'action-invocation-counter)))

           ;; Call our first action
           (let ((step1 (read-state :previous-state start)))

             ;; We should remain in the same session
             (is (same-session-p start step1))

             ;; Now that we've called an action, let's check the altered state.
             (is (eql 1 (getf step1 'action-invocation-counter)))
             (is (eql 1 (getf step1 'backtracked-slot)))
             (is (eql 1 (getf step1 'not-backtracked-slot)))
             (is (eql 1 (getf step1 'slot-for-default-backtrack-test)))

             ;; By Default, UCW should have created a new frame
             (is (not (same-frame-p start step1)))

             (let ((start-frame (read-state :previous-state start)))

               (is (same-session-p start start-frame))

               ;; So when calling the start frame again, by default:
               (is (eql 2 (getf start-frame 'action-invocation-counter)))
               (is (eql 1 (getf start-frame 'backtracked-slot)))
               (is (eql 2 (getf start-frame 'not-backtracked-slot)))
               (is (eql (if default-backtrack 1 2)
                        (getf start-frame 'slot-for-default-backtrack-test))))

             (let ((step1-frame (read-state :previous-state step1)))

               (is (eql 3 (getf step1-frame 'action-invocation-counter)))
               (is (eql 2 (getf step1-frame 'backtracked-slot)))
               (is (eql 3 (getf step1-frame 'not-backtracked-slot)))
               (is (eql (if default-backtrack 2 3)
                        (getf step1-frame 'slot-for-default-backtrack-test))))))

      ;; Get rid of our session
      (when session-id-to-delete
        (delete-test-session session-id-to-delete)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Let's test some permanent link functionality: UPDATE-URL, COMPUTE-URL.
;;;
;;; We put the counter slot in the url, so when it's bookmarked and opened
;;; again without a valid session the value is restored.

;; muffle some warnings while loading
(defcomponent url-rewriting-test-component ()
  ())

(defgeneric action-href-of (x))
(defgeneric (setf action-href-of) (v x))
(defgeneric counter-of (x))
(defgeneric (setf counter-of) (v x))
(defgeneric message-of (x))
(defgeneric (setf message-of) (v x))

(defixture ensure-url-rewrite-test-entry-point
  (:setup
   (ensure-test-application-with-action-dispatcher :setup)
   (finishes
     (with-test-compiler-environment
       (defcomponent url-rewriting-test-component ()
         ((counter
           :initform 0
           :initarg :counter
           :backtrack nil ; no, thanks, we'll put it in the url param
           :accessor counter-of)
          (action-href
           :accessor action-href-of))
         (:render (self)
           (setf (action-href-of self) (action-href
                                        (register-action (:class 'basic-action)
                                          (incf (counter-of self)))))
           (render-component-as-plist self)))

       (defentry-point +url-rewrite-entry-point+ (:application *test-application*)
           (((counter +url-rewrite-parameter-name+) "42"))
         (setf counter (parse-integer counter))
         ;; (break "Call'ing with counter value ~A, context is ~A" counter (context.request *context*))
         (call 'url-rewriting-test-component :counter counter))

       (defmethod update-url ((self url-rewriting-test-component) uri)
         (add-query-parameter-to-uri uri
                                     +url-rewrite-parameter-name+
                                     (counter-of self))
         (append-path-to-uri uri +url-rewrite-entry-point+)
         uri))))
  (:teardown
   (ensure-test-application-with-action-dispatcher :teardown)))




