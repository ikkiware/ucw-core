;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw.core)

;; TODO add portable asserts
(defun session-lock-held-p (session)
  #+sbcl
  (eq (sb-thread::mutex-value (lock-of session)) (current-thread)))

(defun assert-session-lock-held (session)
  #+sbcl
  (assert (session-lock-held-p session) ()
          "You must have a lock on the session here"))

(defun application-lock-held-p (application)
  #+sbcl
  (eq (sb-thread::mutex-value (lock-of application)) (current-thread)))

(defun assert-application-lock-held (application)
   #+sbcl
  (assert (application-lock-held-p application) ()
          "You must have a lock on the application here"))

(defmacro with-lock-held-on-server (server &body body)
  `(multiple-value-prog1
      (progn
        (ucw.rerl.threads.dribble "Entering with-lock-held-on-server for server ~S in thread ~S" server (current-thread))
        (with-recursive-lock-held ((lock-of server))
          ,@body))
    (ucw.rerl.threads.dribble "Leaving with-lock-held-on-server for server ~S in thread ~S" server (current-thread))))

(defmacro with-lock-held-on-application (application &body body)
  ;; TODO KLUDGE this is a concurrent access itself and should be wrapped in a (debug-only* ...)
  `(progn (iter (for (nil session) :in-hashtable (application.session-table ,application))
#+nil        (assert (not (session-lock-held-p session)) ()
                "You are trying to lock the application ~A while one of its session ~A is locked by you"
                ,application session))
  (multiple-value-prog1
      (progn
        (ucw.rerl.threads.dribble "Entering with-lock-held-on-application for app ~S in thread ~S" ,application (current-thread))
        (with-recursive-lock-held ((lock-of ,application))
          ,@body))
    (ucw.rerl.threads.dribble "Leaving with-lock-held-on-application for app ~S in thread ~S" ,application (current-thread)))))

(defmacro with-lock-held-on-session (session &body body)
  (rebinding (session)
    `(multiple-value-prog1
       (progn
         (ucw.rerl.threads.dribble "Entering with-lock-held-on-session for ~S in thread ~S" ,session (current-thread))
         (with-recursive-lock-held ((lock-of ,session))
           ,@body))
       (ucw.rerl.threads.dribble "Leaving with-lock-held-on-session for ~S in thread ~S" ,session (current-thread)))))

(defmacro with-session-variables ((&rest names) &body body)
  "Create local bindings for the listed NAMES and set them to
\(session.value ',name session\). If BODY gracefully completes then
save the values of the local variables back into the session."
  (with-unique-names (session)
    `(let ((,session (context.session *context*)))
      (let (,@(iter (for name in names)
                    (collect `(,name (session.value ',name ,session)))))
        (multiple-value-prog1
            (progn
              ,@body)
          ,@(iter (for name in names)
                  (collect `(setf (session.value ',name ,session) ,name))))))))

(defun send-redirect (target &optional (response *response*))
  (clear-response response)
  (setf (get-header response "Status") +http-moved-temporarily+
        (get-header response "Location") target)
  (format (html-stream *response*)
	  "<html><head><title>302 - Redirect</title></head><body><p>Page has moved to <a href=\"~A\"> ~A</a></p> </body></html>"
	  (escape-as-html target) (escape-as-html target)))


(defun open-session-specific-temporary-file (&key (element-type :default)
                                                             (external-format :default))
  (let* ((session (context.session *context*))
         (base (strcat "/tmp/"
                       (session.id session)
                       ".tmp.")))
    (iter (for i :from 0)
          (for name = (strcat base i))
          (awhen (open name
                       :if-exists nil
                       :direction :io
                       :element-type element-type
                       :external-format external-format)
            (return (values it name))))))

(defmacro with-session-specific-temporary-file ((stream &key
                                                             (element-type :default)
                                                             (external-format :default))
                                                      &body body)
  (let ((file-name 'ignored))
    (when (consp stream)
      (assert (= (length stream) 2))
      (setf file-name (second stream))
      (setf stream (first stream)))
    `(multiple-value-bind (,stream ,file-name)
         (open-session-specific-temporary-file :element-type ',element-type :external-format ,external-format)
       ,(when (eq file-name 'ignored)
          `(declare (ignore ignored)))
       (unwind-protect
            (progn
              ,@body)
         (when ,stream
           (unless (ignore-errors
                     (delete-file ,stream)
                     t)
             (ucw.rerl.warn "Failed to delete temporary file in WITH-SESSION-SPECIFIC-TEMPORARY-FILE."))
           (when (open-stream-p ,stream)
             (unless (ignore-errors
                       (close ,stream)
                       t)
               (ucw.rerl.warn "Failed to close temporary file in WITH-SESSION-SPECIFIC-TEMPORARY-FILE."))))))))

