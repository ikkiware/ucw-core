(in-package :ucw-test)

(defsuite* (test/server :in test))

(defclass test-server (standard-server)
  ())

(defmethod handle-request :around ((server test-server) request response)
  "Handle a special test case to ensure the server is working.
This also tests: 
CALL-AS-REQUEST-HANDLER & 
CALL-AS-RESPONSE-HANDLER"
  (if (string= (query-path request) "/brillant.ucw")
      (call-as-request-handler
       server request response
       (lambda ()
         (call-as-response-handler
          response
          (lambda ()
            (out "BRILLANT!"))
          :send-response t)))
      (call-next-method)))

(defvar *running-test-servers* (list))

(defun make-test-backend (&key (backend *test-backend-type*) (port *test-port*) (host *test-host*))
  (ucw-core::make-backend backend :port port :host host))

(defun make-test-server (&key (server-class 'test-server))
  (make-instance server-class :backend (make-test-backend)))

(defun startup-test-server (&optional (server *test-server*))
  (finishes
    (is (not (server.started server)))
    (startup-server server)
    (is (server.started server))
    (pushnew server *running-test-servers*)
    (setf *test-server* server)))

(defun restart-test-server (&optional (server *test-server*))
  (finishes
    (restart-server server)))

(defun shutdown-test-server (&optional (server *test-server*))
  (finishes
    (is (not (null server)))
    (is (server.started server))
    (shutdown-server server)
    (is (not (server.started server)))
    (setf *running-test-servers* (delete server *running-test-servers*))
    (when (eq server *test-server*)
      (setf *test-server* nil))
    (values)))

(defun cleanup-test-environment ()
  (finishes
    ;; make sure that we're not holding ports
    (mapcar 'shutdown-test-server *running-test-servers*)
    (is (zerop (length *running-test-servers*)))
    (is (null *test-server*))))

(defixture ensure-test-server
  (:setup
   (startup-test-server (make-test-server)))
  (:teardown
   (shutdown-test-server)))

;; we don't call this test automatically because it can produce transient
;; address in use errors.
(deftest (test/server/startup-shutdown :auto-call nil) ()
  "Startup the server, and restart it for good measure."
  (with-fixture ensure-test-server
    (test-backend-listening)
    (restart-test-server)
    (test-backend-listening))
  (signals error (test-backend-listening)))

(defun test-backend-listening ()
  (finishes (web "" :expected-status nil)))

(deftest test/server/listens-and-asnwers ()
  "The test server should return a string when queried"
  (with-fixture ensure-test-server
    (test-backend-listening)
    (is (string= "BRILLANT!" (web "brillant.ucw")))))

