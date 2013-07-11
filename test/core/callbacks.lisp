(in-package :ucw-test)

(defsuite* (test/callback :in test) ()
  (with-fixture ensure-test-application-with-action-dispatcher
    (run-child-tests)
    (test/action/remote-action :default-backtrack nil)
    (test/action/remote-action :default-backtrack t)
    (test/action/remote-action :default-backtrack #'identity)))

(defun %callback-maker (self)
  (lambda (value)
    (error "~%Setting message to ~A~%" value)
    (setf (message self) value)))

(deftest test/action/define-test-callbacks-component (&key (default-backtrack nil))
  (with-fixture ensure-test-application
    (finishes
      (with-test-compiler-environment
        (eval `(defcomponent test-callbacks-component (test-component)
                ((renderer
                  :initarg :renderer
                  :initform nil
                  :accessor renderer-of)
                 (callback-maker
                  :initarg :callback-maker
                  :initform '%callback-maker
                  :accessor callback-maker-of))
                (:entry-point "test-callbacks.ucw" (:application *test-application*))
                (:default-backtrack ,default-backtrack)
                (:render (self)
                 (if (renderer-of self)
                     (funcall (renderer-of self) self)
                     (out "~S" `(message ,(message self)
                                 backtracked-slot ,(backtracked-slot-of self)
                                 not-backtracked-slot ,(not-backtracked-slot-of self)
                                 session ,(session.id  (context.session *context*))
                                 frame ,(frame.id (context.current-frame *context*))
                                 callback ,(register-callback
                                            (funcall (callback-maker-of self) self))
                                 set-callback ,(register-callback
                                                (lambda (value)
                                                  (setf (callback-maker-of self)
                                                        (eval (read-from-string value)))))
                                 callback-maker ,(callback-maker-of self)
                                 ;;url ,(print-uri-to-string (compute-url *test-application* :action-id "asd"))
                                 ))))))))))







