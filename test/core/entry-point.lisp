(in-package :ucw-test)

(defsuite* (test/entry-point :in test) ()
  (with-fixture ensure-test-application
    (run-child-tests)))

(defparameter +default-entry-point-answer+ "Test Entry Point? Brilliant!")
(defparameter +ugly-url-argument+ " +!@#$^&%*[](){}")
(defparameter +url-argument-with-non-ascii-characters+ "disabled for now") ;;"éáóüö") ;; TODO puri does not properly escape non-ascii

(defixture ensure-test-entry-points
  (:setup
   (ensure-test-application :setup)
   (defentry-point "simple-entry.ucw" (:application *test-application* :class url-dispatcher) ()
     (out +default-entry-point-answer+))
   ;; :class url-dispatcher should be the default, so leave it out
   (defentry-point "simple-entry-with-args.ucw" (:application *test-application*)
       ((arg1 "Test Entry Point?") (arg2 "Brilliant!"))
     (out "~A ~A" arg1 arg2)))
  (:teardown
   (setf (application.dispatchers *test-application*)
         (delete-if (lambda (el)
                      (and (typep el 'url-matcher)
                           (or (string= (url-string el) "simple-entry.ucw")
                               (string= (url-string el) "simple-entry-with-args.ucw"))))
                    (application.dispatchers *test-application*)))
   (ensure-test-application :teardown)))

(defapptest test/entry-point/remote-entry-point-test ()
  (labels ((dispatcher-count ()
             (length (application.dispatchers *test-application*)))
           (dispatcher-count-is (count)
             (is (= (dispatcher-count) count))))
    (let ((original-dispatcher-count (dispatcher-count)))
      (with-fixture ensure-test-entry-points
        (dispatcher-count-is (+ original-dispatcher-count 2))
        (is (string= +default-entry-point-answer+
                     (web (uri "simple-entry.ucw"))))
        (is (string= +default-entry-point-answer+
                     (web (uri "simple-entry-with-args.ucw"))))
        (is (string= (strcat +ugly-url-argument+ " Brilliant!")
                     (web (uri "simple-entry-with-args.ucw"
                               "arg1" +ugly-url-argument+))))
        (is (string= (strcat +ugly-url-argument+ " " +url-argument-with-non-ascii-characters+)
                     (web (uri "simple-entry-with-args.ucw"
                               "arg2" +url-argument-with-non-ascii-characters+
                               "arg1" +ugly-url-argument+)))))
      (dispatcher-count-is original-dispatcher-count))))

