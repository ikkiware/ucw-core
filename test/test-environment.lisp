(in-package :ucw-test)

;(enable-bracket-syntax)

(asdf:oos 'asdf:load-op :ucw.httpd) ; This is here to be able to comment it out from MAKE-BACKEND to speed up running the tests

(in-root-suite)

(defmacro with-ucw-logger-level (log-level &body body)
  `(with-logger-level (ucw-core::ucw) ,log-level
    ,@body))

(defsuite* test (&key (log-level +warn+))
  (with-ucw-logger-level log-level
    (with-fixture ensure-test-server
      (run-child-tests))))

(defmacro deftest (name args &body body)
  `(stefil:deftest ,name ,args
    ;; rebind *test-application*, so that we can setf it freely in the tests
    (rebind (*test-application* *test-server*)
      ,@body)))

(defvar *muffle-compiler-warnings* t
  "Muffle or not the compiler warnings while the tests are running.")

(defvar *test-server* nil
  "The currently running test server.")

#+nil(defvar *test-backend* nil
  "The currently running test backend.")

(defvar *test-application* nil
  "The currently running test application.")

(defparameter *test-backend-type* :httpd)
(defparameter *test-host*         nil) ;; NIL means the default
(defparameter *test-port*         9010)

(define-symbol-macro +test-server-base-url+
  (strcat "http://" (or *test-host* "localhost") ":" *test-port* "/"))

(defun out (string &rest args)
  (let ((*package* #.(find-package :common-lisp)))
    (apply 'format (html-stream ucw-core::*response*) string args)))

(defgeneric web (path &key request-parameters expected-status parse-result-as)
  (:method ((path string) &key request-parameters (expected-status 200) parse-result-as)
    (multiple-value-bind (body status headers url)
        (drakma:http-request (strcat (when (or (< (length path) 4)
                                               (not (string= (subseq path 0 4) "http")))
                                       +test-server-base-url+)
                                     (if (and (> (length path) 0)
                                              (char= (elt path 0) #\/))
                                         (subseq path 1)
                                         path))
                             :parameters request-parameters)
      (declare (ignore url))
      (when expected-status
        (is (= status expected-status)))
      (when parse-result-as
        (setf body (ecase parse-result-as
                     (:sexp (read-from-string body))
                     ((:xmls :dom)
                      (flexi-streams:with-input-from-sequence
                          (stream (flexi-streams:string-to-octets body :external-format :utf-8))
                        (cxml:parse-stream stream (ecase parse-result-as
                                                    (:dom (cxml-dom:make-dom-builder))
                                                    (:xmls (cxml-xmls:make-xmls-builder)))))))))
      (values body status headers)))
  
  (:method ((path ucw-core:uri) &rest args)
    (apply 'web (print-uri-to-string path) args)))

(defun uri (&optional (path "") &rest query-params)
  (let ((concatenated-path (apply #'strcat (ensure-list path))))
    (make-uri
     :scheme "http"
     :host (or *test-host* "localhost")
     :port *test-port*
     :path (strcat (application.url-prefix *test-application*)
            (if (and (not (zerop (length concatenated-path)))
                     (char= (elt concatenated-path 0) #\/))
                (subseq concatenated-path 1)
                concatenated-path))
     :query (iter (for (name value) :on query-params :by #'cddr)
                  (collect (cons name value))))))

(defmacro with-test-compiler-environment (&body body)
  `(handler-bind ((style-warning
                   (lambda (c)
                     (when *muffle-compiler-warnings*
                       (muffle-warning c)))))
    ,@body))

(defun render-component-as-plist (component)
  (let ((*print-readably* t))
    (out "~S"
         (append
          (iter (for slot :in (class-slots (class-of component)))
                (for slot-name = (slot-definition-name slot))
                (when (and (member (symbol-package slot-name)
                                   (list (find-package :ucw)
                                         (find-package :ucw-test)))
                           (slot-boundp component slot-name))
                  (let ((slot-value (slot-value component slot-name)))
                    (when (ignore-errors
                            (let ((*print-readably* t))
                              (with-output-to-string (str)
                                (print slot-value str)))
                            t)
                      (nconcing (list slot-name slot-value))))))
          `(session-id ,(session.id  (context.session *context*))
            frame-id ,(frame.id (context.current-frame *context*)))))))

(defun render-component-as-xml (component)
  (let ((*print-readably* t))
    {with-xml-syntax
      <component
        <session-id (princ (session.id  (context.session *context*)) *yaclml-stream*)>
        <frame-id (princ (frame.id (context.current-frame *context*)) *yaclml-stream*)>
        (iter (for slot :in (class-slots (class-of component)))
              (for slot-name = (slot-definition-name slot))
              (when (and (member (symbol-package slot-name)
                                 (list (find-package :ucw)
                                       (find-package :ucw-test)))
                         (slot-boundp component slot-name))
                (let* ((slot-value (slot-value component slot-name))
                       (slot-value-string (ignore-errors
                                            (let ((*print-readably* t))
                                              (with-output-to-string (str)
                                                (print slot-value str))))))
                  (when slot-value-string
                    <(progn (string-downcase slot-name))
                      (princ slot-value-string *yaclml-stream*)>))))>}))

(defun make-invocation-id ()
  (princ-to-string
   (mod (get-internal-real-time)
        1000000)))
