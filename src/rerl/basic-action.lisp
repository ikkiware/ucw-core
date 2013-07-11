;; -*- lisp -*-

(in-package :it.bese.ucw.core)

(defmethod (setf action-lambda-of) :after (value action)
  "Make sure the FUNCALLABLE-INSTANCE-FUNCTION is in sync with the LAMBDA"
  (c2mop:set-funcallable-instance-function action value))

(defmethod initialize-instance :after ((action action) &key)
  ;; this little hack ensures that #'(SETF ACTION-LAMBDA-OF) is called,
  ;; and our :after method on it sets the funcallable instance function
  (setf (action-lambda-of action) (action-lambda-of action)))


(defmethod handle-action ((action action) application session frame)
  "The base method simply starts the CALL-ACTION protocol."
  (call-action action application session frame))

(defun abort-action (&optional failure-message)
  "Calling it without arguments means a graceful abort, while providing a failure message means there was an error.
An ajax request will for example show the FAILURE-MESSAGE in one way or another when provided."
  (ucw.rerl.actions.debug "Invoking the ABORT-ACTION restart with FAILURE-MESSAGE ~S" failure-message)
  (let ((abort-action-restart (find-restart 'abort-action)))
    (assert abort-action-restart)
    (invoke-restart abort-action-restart failure-message)))

(defmethod call-action :around ((action action) application session frame)
  "Install an error handler that redirects all errors to the HANDLE-TOPLEVEL-CONDITION protocol."
  (call-with-ucw-error-handler
   (lambda ()
     (restart-case
         (call-next-method)
       (abort-action (&optional (failure-message "Internal server error"))
         :report "Abort processing this action"
         (declare (ignorable failure-message))
         (ucw.rerl.actions.debug "Outermost ABORT-ACTION restart invoked with FAILURE-MESSAGE ~S" failure-message))))
   (lambda (error)
     (ucw.rerl.actions.debug "Action ~A of application ~A captured a request error:~%~A"
                             action application error)
     (handle-toplevel-condition application error action)
     (abort-backend-request error))))

(defmethod call-action ((action action) application session frame)
  "The base method simply funcalls the action."
  (funcall action))

(defmethod handle-action :before ((action action-with-isolation-support) application session frame)
  (ucw.rerl.actions.debug "Action ~S valid in current session? ~A" action (action-valid-p action)))

(defmethod reinstate-backtracked-using-action (frame (action action-with-isolation-support))

  (when (action-backtracks action)
    (setf (effective-backtracks-of frame) (action-backtracks action)))
  (call-next-method))

(defmethod save-backtracked-using-action (frame (action action-with-isolation-support))

  (prog1  (call-next-method)
    (setf (action-backtracks action) (clone-effective-backtracks (context.session *context*) frame))))

(defmethod call-action ((action action-with-isolation-support) application session frame)
  ;; don't use :around to let other customizations
  ;; do whatever they want in their :before/:after methods
  
  
  (if (action-isolated-p action)
      (cond ((action-valid-p action) 
	     (prog1 (call-next-method) 
)))
      (call-next-method)))

(defmethod make-new-frame :around ((action action-with-isolation-support) session)
  (if (action-valid-p action)
      (call-next-method)
      (session.current-frame session)))

(defmethod make-new-frame :around ((action basic-action) session)
  (if (or (action-make-new-frame-p action)
          (not (session.current-frame session)))
      (progn
        (ucw.rerl.actions.debug "Ensuring new frame for action ~S" action)
        (call-next-method))
      (session.current-frame session)))

(defmethod call-callbacks :around ((action basic-action) frame request)
  (when (action-call-callbacks-p action)
    (ucw.rerl.actions.debug "Calling callbacks of action ~S" action)
    (call-next-method)))

(defmethod call-action :before ((action basic-action)
                                application
                                (session basic-session)
                                previous-frame)
  (when previous-frame
    (ucw.rerl.actions.dribble "Starting CALL-CALLBACKS protocol for action ~S" action)
    (call-callbacks action previous-frame (context.request *context*)))
  (make-new-frame action session))

(defmethod call-render ((action basic-action) application session frame)
  ;; nb: don't use the same frame as returned by make-new-frame
  ;; to get the current component. 99% of the time they will be
  ;; the same as below, but 1% they will be differenet and that
  ;; 1% of the time is when all hell has broken lose and we're
  ;; dancing with cats...
  (when (action-call-render-p action)
    (awhen (frame.window-component (context.current-frame *context*))
      (loop named render-loop do
            (restart-case
             (progn
               (render it)
               (return-from render-loop))
             (retry ()
                    :report "Retry calling RENDER."))))))

;; TODO (attila) : in a customized CALL-RENDER we could report if
;; (not (action-valid-in-current-request-p action))
;; we should mark in the action that the first invocation of an isolated action is still running
;; and answer a page telling this fact to the user. then keep reloading that page until the
;; first invocation has finished and recorded the response in the action-entry.
;; then all subsequent invocations should blindly return that response without calling
;; the action or the render loop again.

(defmethod handle-action :wrap-around (action application (session basic-session) frame)
  (with-thread-name (concatenate 'string
                                 " / HANDLE-ACTION of "
                                 (session.id session))
    (with-lock-held-on-session session
      (call-next-method))))

(defmethod handle-action :around (action application (session basic-session) frame)
  (assert (not (application-lock-held-p application)))
  (assert-session-lock-held session)
  ;; we call SESSION-VALID-P this late to be inside possible HANDLE-ACTION :around customizations.
  ;; if we find the session expired (possibly due to some user customizations), then
  ;; signal a SESSION-NOT-FOUND error to instruct the parents on the stack to delete this session
  ;; and retry.
  (unless (session-valid-p session)
    (error 'session-is-invalid :session session))
  (call-next-method))

(defmethod handle-action :wrap-around ((action action) application (session basic-session) frame)
  (setf (context.action *context*) action)
  (call-next-method))

(defmethod handle-action ((action action)
                          application
                          (session basic-session)
                          previous-frame)
  (ucw.rerl.actions.debug "Starting CALL-ACTION protocol for action ~S" action)
  (call-action action application session previous-frame))


(defgeneric reinstate-backtracked-using-action (frame action)
  (:method (frame action )
    (declare (ignore action))
    (reinstate-backtracked frame)))

(defgeneric save-backtracked-using-action (frame action)
  (:method (frame action )
    (declare (ignore action))
    (save-backtracked frame)))

(defmethod handle-action :around ((action basic-action)
                          application
                          (session basic-session)
                          previous-frame)
  (assert (and session (eq session (context.session *context*))))
  (when (and previous-frame
             (not (eq previous-frame (context.current-frame *context*))))
    (setf (context.current-frame *context*) previous-frame)
    (ucw.rerl.actions.debug "Reinstating backtracked places to frame ~S" previous-frame)
    (reinstate-backtracked-using-action previous-frame action))

  (call-next-method)

  (let ((current-frame (context.current-frame *context*)))
    (ucw.rerl.actions.debug "Starting CALL-RENDER protocol for action ~S" action)
    (call-render action application session current-frame)

    (ucw.rerl.actions.debug "Saving backtracked places into frame ~S" current-frame)
    (save-backtracked-using-action (context.current-frame *context*) action)))

(defun make-action (lambda &rest initargs &key (class *default-action-class*) &allow-other-keys)
  "Makes a new unregistered action."
  (remf-keywords initargs :class)
  (apply #'make-instance class :lambda lambda initargs))

(defmacro make-action-body ((&rest args &key with-call/cc &allow-other-keys) &body body)
  (remf-keywords args :with-call/cc)
  `(make-action (lambda ()
                  ,@(if with-call/cc
                        `((with-call/cc
                            ,@body))
                        body))
    ,@args))

;;; ** Call, Answer and Defentry-Point

;;;; Binding request params to variables

(defmacro with-request-params (request-lambda-list request &body body)
  "Bind, according the REQUEST-LAMBDA-LIST the parameters in
  REQUEST and execute BODY.

REQUEST-LAMBDA-LIST is a list of the form:

 ( [ ( symbol string ) | symbol ]
   [ default-value [ supplied-symbol-name ]? ]? )

If the request contains a param (no distinction between GET and
POST params is made) named STRING (which defaults to the symbol
name of SYMBOL) the variable SYMBOL is bound to the associated
value (which is always a string) . If no parameter with that name
was passed SYMBOL will be bound to DEFAULT-VALUE and the variable
named SUPPLIED-SYMBOL-NAME will be bound to NIL.

NB: Parameter names are matched case insensitively."
  (gen-request-param-binder request-lambda-list request body))

(defstruct arg-spec
  symbol
  name-string
  default-value
  supplied-symbol-name)

(defun gen-request-param-binder (args request body)
  (let ((args (mapcar (lambda (arg-spec)
                        (destructuring-bind ((name-symbol &optional name-string) &optional default-value supplied-p)
                            arg-spec
                          (make-arg-spec :symbol name-symbol
                                         :name-string (if name-string
                                                          name-string
                                                          (string-downcase (string name-symbol)))
                                         :default-value default-value
                                         :supplied-symbol-name supplied-p)))
                      (mapcar (lambda (arg-spec)
                                ;; normalize the arg specs
                                (cons (ensure-list (car (ensure-list arg-spec)))
                                      (cdr (ensure-list arg-spec))))
                              args))))
    (let ((req (gensym)))
      `(let ((,req ,request))
         (declare (ignorable ,req))
         (let
             ;; bind the vars to their default values
             ,(iterate
                 (for arg-spec in args)
                 (collect (list (arg-spec-symbol arg-spec)
                                (arg-spec-default-value arg-spec)))
                 (awhen (arg-spec-supplied-symbol-name arg-spec)
                   (collect (list it nil))))
           ,@(mapcar (lambda (arg-spec)
                       `(awhen (get-parameter ,req ,(arg-spec-name-string arg-spec))
                          (setf ,(arg-spec-symbol arg-spec) it)
                          ,(awhen (arg-spec-supplied-symbol-name arg-spec)
                             `(setf ,it t))))
                   args)
         ,@body)))))

;;;; Defining actions and entry points

(defmacro defentry-point (url
                          (&key (application '*default-application*)
                                (class 'url-dispatcher)
                                (priority nil priority-p)
                                (action-options nil)
                                (with-call/cc t))
                          request-lambda-list &body body)
  "Define an entry point bound to the url URL of type CLASS.

URL must be a string which, when appended to APPLICATION's
url-prefix, specifies the complete url for this entry
point. CLASS is a symbol naming the dispatcher class.

APPLICATION (evaluated) specifies the application object this
entry point will be attached to. If NIL *default-application*
will be used, otherwise it must be the name of an existing
application.

REQUEST-LAMBDA-LIST is a request lambda list as per
WITH-REQUEST-PARAMS.

The body of the entry-point is executed whenever the server
recieves a request for the specified url. The body can contain
calls to components but must not contain answer forms.

If the backend supports it then the url is
automatically registered with the backend, otherwise (mod_lisp)
developers must manually configure the backend."
  (with-unique-names (app)
    (rebinding (url)
      `(let ((,app ,application))
         (assert (stringp ,url) (,url)
                 "Entry point urls must be strings, ~S is not allowed." ,url)
         (if ,app
             (setf
              ;; first delete any entry points of type CLASS with
              ;; url-string URL
              (application.dispatchers ,app)
              (delete-if (lambda (ep)
                           (and (eql (class-name (class-of ep)) ',class)
                                (string= ,url (slot-value ep 'url-string))))
                         (application.dispatchers ,app))
              ;; now add the entry point
              (application.dispatchers ,app)
              (append (application.dispatchers ,app)
                      (list
                       (make-instance ',class
                                      ,@(when priority-p `(:priority ,priority))
                                      :url-string ,url
                                      :handler ,(let ((handler `(let ((self nil))
                                                                 (declare (ignorable self))
                                                                 (with-request-params ,request-lambda-list
                                                                     (context.request *context*)
                                                                   ,@body))))
                                                     `(make-action ,(if with-call/cc
                                                                        `(lambda ()
                                                                          (with-call/cc
                                                                            ,handler))
                                                                        `(lambda ()
                                                                          ,handler))
                                                                   ,@action-options))))))
             (error "No application specified and *DEFAULT-APPLICATION* is NIL."))
         ,url))))

(defun action-href (action
                                &key (component (context.window-component *context*))
                                (frame (context.current-frame *context*)))
  "Given an action returns an URL (as a string) which will call
the action lambda.

The COMPONENT parameter is passed directly to COMPUTE-URL, FRAME
is passed to REGISTER-ACTION. ACTION may be a lambda
or an action object made with MAKE-ACTION factory methods."
  (declare (type (or action function) action))
  (let ((uri (compute-url (etypecase action
                            (action (unless (action-id action)
                                      (register-action-in-frame frame action))
                                    action)
                            (function (register-action (:frame frame :with-call/cc nil)
                                        (funcall action))))
                          component)))
    (values (print-uri-to-string uri) uri)))

(defmacro action-href-body ((&rest args &key (component '(context.window-component *context*))
                                        (frame '(context.current-frame *context*))
                                        &allow-other-keys)
                                  &body body)
  (remf-keywords args :frame :component)
  `(action-href (register-action ,args ,@body) :component ,component :frame ,frame))

(defmacro download-action-href-body ((&key file-name) &body body)
  (with-unique-names (uri)
    (rebinding (file-name)
      `(bind (((:values nil ,uri) (action-href
                                   (register-action (:call-render nil
                                                     :invocation-isolated nil
                                                     :make-new-frame nil)
                                     ,@body))))
         (append-path-to-uri ,uri ,file-name)
         (print-uri-to-string ,uri)))))

(defmacro abort-raw-request ()
  `(progn
     (ucw.rerl.actions.debug "Aborting raw request")
     (throw 'abort-raw-request nil)))

(defmacro handle-raw-request ((&key (content-type "application/octet-stream")
                                         (http-status-code +http-ok+)
                                         content-disposition
                                         expires-in-ut
                                         max-age-in-secs
                                         (send-headers t)
                                         (cache (or expires-in-ut max-age-in-secs))
                                         (with-network-stream nil)
                                         (error-handler `(render-standard-error-page))
                                         (with-yaclml-stream nil))
                                    &body body)
  "This macro handles a raw request. Will set the http cache control headers
according to the supplied arguments, send the headers, execute the BODY and
close the request. WITH-NETWORK-STREAM will be bound to the network stream
unless it's nil. When WITH-YACLML-STREAM is not nil the body will run inside a
with-yaclml-stream block and after it finished it will be properly encoded
and written into the network stream. Additionally if it is a symbol then
it will be bound with that name."
  (with-unique-names (ok)
    `(let* (,@(when with-network-stream
                    `((,with-network-stream (network-stream *request*))))
            ,@(when with-yaclml-stream
                    (when (eq with-yaclml-stream t)
                      (setf with-yaclml-stream (gensym "YACLML-STREAM")))
                    ;; TODO OPTIMIZE check for *print-pretty* and the make-string-output-stream interaction
                    `((,with-yaclml-stream (make-string-output-stream)))))
       (setf (response-managed-p *response*) nil)
       (setf (get-header *response* "Status") ,http-status-code
             (get-header *response* "Content-Type") ,content-type)
       ,(when content-disposition
              `(setf (get-header *response* "Content-Disposition") ,content-disposition))
       ,(if cache
            `(progn
               ,@(when max-age-in-secs
                       `((setf
                          (get-header *response* "Cache-Control")
                          (strcat "max-age=" (princ-to-string ,max-age-in-secs)))))
               ,@(when expires-in-ut
                       `((setf
                          (get-header *response* "Expires")
                          (date:universal-time-to-http-date ,expires-in-ut)))))
            `(setf
              (get-header *response* "Cache-Control") "no-cache, no-store"
              (get-header *response* "Expires") #.(date:universal-time-to-http-date
                                                   3000000000))) ; somewhere in 1995
       (catch 'abort-raw-request
         (handler-bind ((stream-error (lambda (c)
                                        (when (eq (stream-error-stream c)
                                                  (network-stream *request*))
                                          (abort-raw-request)))))
           (let ((,ok nil))
             (unwind-protect
                  ,(if with-yaclml-stream
                       `(multiple-value-prog1
                            (yaclml::with-yaclml-stream ,with-yaclml-stream
                              ,@body)
                          (let ((content (string-to-octets (get-output-stream-string ,with-yaclml-stream)
                                                           (encoding *response*))))
                            (ucw.rerl.actions.debug "Flushing ~A bytes of yaclml content from HANDLE-RAW-REQUEST" (length content))
                            (setf (get-header *response* "Content-Length") (princ-to-string (length content)))
                            ,(when send-headers
                               `(send-headers *response*))
                            (setf ,ok t)
                            (write-sequence content (network-stream *request*))))
                       `(progn
                          ,(when send-headers
                             `(progn
                                (send-headers *response*)
                                (setf ,ok t)))
                          (multiple-value-prog1
                              (progn
                                ,@body)
                            (setf ,ok t))))
               ,(when error-handler
                  `(unless ,ok
                     ,error-handler)))))))))

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
