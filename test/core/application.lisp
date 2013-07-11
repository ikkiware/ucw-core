(in-package :ucw-test)

(defsuite* (test/application :in test) ()
  (with-fixture ensure-test-server
    (run-child-tests)))

(defun run-application (application &rest initargs)
  (rebind (*test-server* *test-application*)
    (unwind-protect
         (progn
           (startup-test-server (make-test-server))
           (unless (typep application 'application)
             (setf application (apply #'make-instance application initargs)))
           (register-test-application application)
           (break "Your application is running, point your browser to ~A"
                  (print-uri-to-string (uri))))
      (shutdown-test-server))))

(defclass test-application (basic-application)
  ())

(defparameter +application-test-answer+     "Test Application? Brilliant!")
(defparameter +test-application-url-prefix+ "/test-app/")
(defparameter +application-test-path+       (strcat +test-application-url-prefix+ "brilliant.ucw"))

(defmethod service ((app test-application) context)
  "Creates a special case for the remote test 
This also tests CALL-AS-RESPONSE-HANDLER."
  (if (string= (query-path (context.request context)) +application-test-path+)
      (progn
        (call-as-response-handler
         (context.response context)
         (lambda ()
           (out +application-test-answer+))
         :send-response t)
        t)
      (call-next-method)))

(defun make-test-application (&key (application 'test-application))
  (make-instance application
                 :url-prefix +test-application-url-prefix+
                 ;; dispatchers must be nil
                 ;; or are created by default
                 :dispatchers nil))

(defun register-test-application (&optional (app *test-application*) (server *test-server*))
  (finishes
    (let ((app-count (length (server.applications server))))
      (register-application server app)
      (setf *test-application* app)
      (is (eq (application.server app) server))
      (is (= (1+ app-count) (length (server.applications server)))))))

(defun unregister-test-application (&optional (app *test-application*) (server *test-server*))
  (finishes
    (let ((app-count (length (server.applications server))))
      (unregister-application server app)
      (is (eq (application.server app) server))
      (is (= (1- app-count) (length (server.applications server)))))))

(defixture ensure-test-application
  (:setup
   (ensure-test-server :setup)
   (register-test-application (make-test-application)))
  (:teardown
   (unregister-test-application)
   (ensure-test-server :teardown)))

(deftest test/application/answers ()
  (with-fixture ensure-test-server
    (with-fixture ensure-test-application
      (is (string= +application-test-answer+ (web +application-test-path+))))
    (is (not (string= +application-test-answer+
                      (web +application-test-path+ :expected-status 404))))))

(defmacro defapptest (name args &body body)
  `(deftest ,name ,args
    (with-fixture ensure-test-application
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and application rendering some raw data

(defparameter +raw-test-application-url-prefix+ "/raw-test-app/")
(defparameter +raw-test-application-data+ "nFkiKyBmkuBxhNwPUJaMWUPUrlqZsbZTgrSHUncGVSbJPql")

(defclass raw-test-application (basic-application)
  ((raw-data
    :initform (string-to-octets +raw-test-application-data+ :utf-8)
    :initarg :raw-data
    :accessor raw-data-of))
  (:default-initargs :url-prefix +raw-test-application-url-prefix+))

(defmethod service ((app raw-test-application) context)
  (let ((response (context.response *context*)))
    (call-as-response-handler
     response
     (lambda ()
       (handle-raw-request ()
         (write-sequence (raw-data-of app)
                         (network-stream response))))
     :content-type "application/octet-stream")
    t))

(defixture ensure-raw-test-application
  (:setup
   (ensure-test-server :setup)
   (register-test-application (make-instance 'raw-test-application)))
  (:teardown
   (unregister-test-application)
   (ensure-test-server :teardown)))

(deftest test/action/raw-response ()
  (with-fixture ensure-raw-test-application
    (multiple-value-bind (body status headers)
        (web +raw-test-application-url-prefix+)
      (declare (ignore status))
      (is (string= (cdr (assoc :content-type headers))
                   "application/octet-stream"))
      (is (equalp body
                  (string-to-octets +raw-test-application-data+
                                    (external-format-for :http))))
      (is (not (null (assoc :date headers))))
      (is (not (null (assoc :cache-control headers))))
      (is (not (null (assoc :expires headers))))))
  (values))


