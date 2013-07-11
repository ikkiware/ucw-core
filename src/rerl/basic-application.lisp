;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defmethod (setf application.dispatchers) :around (dispatchers (app basic-application))
  (call-next-method (sort (copy-list dispatchers)
                          #'> :key #'priority)
                    app))

(defun register-dispatcher (application dispatcher)
  (setf (application.dispatchers application)
        (append (ensure-list dispatcher) (application.dispatchers application))))

(defmethod debug-on-error :around ((app basic-application))
  (if (slot-boundp app 'debug-on-error)
      (call-next-method)
      (debug-on-error nil)))

(defmethod request-context-class list ((app minimal-application))
  'standard-request-context)

(defmethod shared-initialize :after ((app minimal-application) slot-names &key &allow-other-keys)
  (setf (request-context-class-of app)
	(c2mop:ensure-class
	 (intern-concat (list (package-name (symbol-package (class-name-of app)))
			      "."
			      (class-name-of app)
			      "-REQUEST-CONTEXT")
			:it.bese.ucw.core.generated)
	 :direct-superclasses (mapcar #'find-class (request-context-class app)))))

(defmethod shared-initialize :after ((app basic-application) slot-names &key &allow-other-keys)
  ;; trigger the accessor to ensure it's sorted
  (setf (application.dispatchers app) (application.dispatchers app)))

(defmethod shared-initialize :after ((app application-with-session-handling-mixin) slot-names &key &allow-other-keys)
  (setf (session-class-of app)
	(c2mop:ensure-class
	 (intern-concat (list (package-name (symbol-package (class-name-of app)))
			      "."
			      (class-name-of app)
			      "-SESSION")
			:it.bese.ucw.core.generated)
	 :direct-superclasses (mapcar #'find-class (session-class app)))))

(defmethod make-request-context ((app minimal-application)
                                 (request request)
                                 (response response))
  (make-instance (request-context-class-of app)
                 :request request
                 :response response
                 :application app))

(defmethod find-session ((application application-with-session-handling-mixin) (context request-context))
  "Returns the session with ID (find-session-id CONTEXT) in APPLICATION,
NIL if there is no session with that id."
  (when-bind session-id (find-session-id context)
    (with-thread-name " / FIND-SESSION"
      ;; TODO it should really be this: (assert-application-lock-held application)
      ;; but it is waiting for the dispatcher refactoring that calls the matchers with the lock held
      (with-lock-held-on-application application
        (let ((session (gethash session-id (application.session-table application))))
          (when session
            (assert (string= session-id (session.id session)))
            (unless (session-expired-p session)
              session)))))))

(defmethod session-class list ((app basic-application))
  'basic-session)

(defmethod make-new-session ((application application-with-session-handling-mixin))
  (ucw.rerl.application.dribble "Trying to make a new session.")
  (assert-application-lock-held application)
  (let ((session-table (application.session-table application)))
    (when (> (hash-table-count session-table)
             *maximum-number-of-sessions*)
      (remove-expired-sessions application)
      (when (> (hash-table-count session-table)
	       *maximum-number-of-sessions*)
	(error 'too-many-sessions)))
    (let* ((session-id (new-random-key session-table +session-id-length+))
           ;; this way the initialize-instance of the session can set the session's lock name properly
           (new-session (make-instance (session-class-of application)
                                       :id session-id)))
      (setf (gethash session-id session-table) new-session)
      (ucw.rerl.application.dribble "New Session id ~S." (session.id new-session))
      new-session)))

(defmethod remove-expired-sessions ((application application-with-session-handling-mixin))
  "Loops over all the sessions in APPLICATION, calls DELETE-SESSION on those for which SESSION-EXPIRED-P returns T and drops them from the APPLICATION's session table."
  (ucw.rerl.application.dribble "Purging expired sesssions of application ~A" application)
  (let ((deleted-sessions (list)))
    (with-lock-held-on-application application
      (iter (for (session-id session) :in-hashtable (application.session-table application))
            (when (session-expired-p session)
              (handler-bind
                  ((serious-condition
                    (lambda (error)
                      (ucw.rerl.application.warn "Could not delete expired session ~A of application ~A, got ~A" session application error)
                      (log-error-with-backtrace error))))
                (delete-session application session)
                (push session deleted-sessions)))))
    (dolist (session deleted-sessions)
      (handler-bind
          ((serious-condition
            (lambda (error)
              (ucw.rerl.application.warn "Error happened while notifying session ~A of application ~A about its exiration, got ~A" session application error)
              (log-error-with-backtrace error))))
        (with-lock-held-on-session session
          (notify-session-expiration session))))
    deleted-sessions))

(defmethod delete-session :around (application session)
  (with-thread-name " / DELETE-SESSION"
    (call-next-method)))

(defmethod delete-session :around ((application minimal-application) (session basic-session))
  ;; it's important to have the application locked, but it's not wise to lock it here: users may want to
  ;; delete sessions, but calling DELETE-SESSION from inside an action (where the session is locked)
  ;; can lead to deadlocks due to violating the (app, session) lock order.
  (assert-application-lock-held application)
  (with-lock-held-on-session session
    (call-next-method)))

(defmethod delete-session ((application application-with-session-handling-mixin) (session session))
  (ucw.rerl.application.dribble "Deleting sesssion ~A of application ~A" session application)
  (remhash (session.id session) (application.session-table application))
  session)

(defmethod notify-session-expiration :before ((session basic-session))
  ;; TODO *context* is not available (assert (not (application-lock-held-p (context.application *context*))))
  (assert-session-lock-held session))

(defmethod notify-session-expiration (session)
  ;; nop
  )

(defmethod ensure-session :around (application context &optional session)
  (declare (ignore session))
  (with-thread-name " / ENSURE-SESSION"
    (call-next-method)))

(defmethod ensure-session ((app application-with-session-handling-mixin)
                           (context standard-request-context)
                           &optional session)
  "If CONTEXT's request specifies a session then put it in the
  context, otherwise create a new context.

Updates the session's last-access time."
  (unless session
    (ucw.rerl.application.dribble "Ensuring new sesssion for application ~A" app)
    (with-lock-held-on-application app
      (setf session (or (find-session app context)
                        (make-new-session app)))))
  (unless (eq (context.session context) session)
    (setf (context.session context) session))
  (setf (session.last-access (context.session context)) (get-universal-time))
  session)

(defun iterate-sessions-with-lock-held (app visitor)
  ;; one way to make it safe from deadlocks is to lock the app first and keep it
  ;; locked until all sessions are visited
  (with-thread-name " / ITERATE-SESSIONS-WITH-LOCK-HELD"
    (prog2
        (ucw.rerl.ajax.dribble "Entering iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread))
        (with-lock-held-on-application app
          (iter (for (nil session) in-hashtable (application.session-table app))
                (with-lock-held-on-session session
                  (funcall visitor session))))
      (ucw.rerl.ajax.dribble "Leaving iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread)))))

(defmethod service ((application basic-application) (context request-context))
  (let ((response (context.response context)))
    (call-as-response-handler
     response
     (lambda ()
       ;; TODO dispatchers are enumerated without a lock on the app, but the current architecture makes
       ;; it hard to lock the app while the dispatcher is selected and release it when the processing
       ;; actually happens. add a proper lock after the dispatching is refactored.
       (flet ((call-dispatchers ()
                (dolist (dispatcher (application.dispatchers application) nil)
                  (when (dispatch dispatcher application context)
                    (ucw.rerl.dispatcher.dribble "~S handled the request, sending response and returning t from app's SERVICE" dispatcher)
                    (send-response response)
                    (return-from call-dispatchers t)))))
         ;; call the dispatchers and if a SESSION-IS-INVALID error comes out then mark
         ;; the session expired and try once again (and let the entry point handlers deal with the request).
         ;; we do this up here, because we may not lock the application while we are down on the call
         ;; stack holding a lock to a session. but also want to leave open the possibility to cancel processing
         ;; requests from user code if a session turns out to be invalid.
         (handler-case
             (call-dispatchers)
           (session-is-invalid (error)
             (mark-session-expired (session-of error))
             (call-dispatchers))))))))

(defprint-object (app application :identity nil)
  (format *standard-output* "~A ~S"
          (application.url-prefix app)
          (hash-table-count (application.session-table app))))

(defmethod startup-application ((app application))
  ;; nop
  )

(defmethod startup-application :before ((app application-with-session-handling-mixin))
  "Simply clears out the app's session-table."
  ;; make sure, just in case...
  (clrhash (application.session-table app)))

(defmethod shutdown-application ((application application))
  ;; nop
  )

(defmethod shutdown-application :around ((application minimal-application))
  (with-thread-name "/ SHUTDOWN-APPLICATION"
    (with-lock-held-on-application application
      (call-next-method))))

(defmethod shutdown-application :before ((app application-with-session-handling-mixin))
  (iter (for (session-id session) :in-hashtable (application.session-table app))
        (delete-session app session)))

(defmethod restart-application ((app basic-application))
  "Calls shutdown-application and then startup-application on
APP."
  (shutdown-application app)
  (startup-application app))

(defmethod compute-url ((action action) (app basic-application))
  "Creates the default url for APP which, when requested, will
cause the action to be called.

The generated URL's path will be the app's url-prefix."
  (let ((query (list (cons +action-parameter-name+
                           (action-id action))))
        (path (application.url-prefix app)))
    (make-uri :path path :query query)))

(defmethod compute-url :around (action (app application-with-session-handling-mixin))
  "Appends the session and frame parameters UCW needs to
find the session and the frame."
  (let ((uri (call-next-method)))
    (add-query-parameter-to-uri uri
                                +session-parameter-name+
                                (session.id (context.session *context*)))
    (add-query-parameter-to-uri uri
                                +frame-parameter-name+
                                (frame.id (context.current-frame *context*)))
    uri))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
