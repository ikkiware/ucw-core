;;; -*- lisp -*-

;;;; * The Packages

(in-package :it.bese.ucw.system)

(defpackage :it.bese.ucw.loggers
  (:nicknames :ucw-log)
  (:use :common-lisp
	:it.bese.ucw.system)
  (:import-from :it.bese.arnesi #:deflogger #:get-logger #:log.level #:strcat)
  (:export #:ucw.log-level))

(defpackage :it.bese.ucw.core
  (:nicknames :ucw-core)
  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)
  (:use :common-lisp
        :it.bese.ucw.system
	:it.bese.ucw.loggers
        :it.bese.arnesi
        :bordeaux-threads
        :local-time
        :trivial-garbage
	:yaclml
        :iterate)
  (:shadow
   #:parent)
  (:export
   ;; Backends and servers
   #:make-backend
   #:*default-server*
   #:standard-server
   #:startup-server
   #:shutdown-server
   #:restart-server
   #:server.applications
   #:server.started
   #:server.backend
   #:debug-on-error
   #:create-server

   ;; requests and responses
   #:request
   #:response
   #:request-context
   #:request-context-class
   #:standard-request-context
   #:handle-request
   #:handle-raw-request
   #:close-request
   #:parameters
   #:call-as-request-handler
   #:get-header
   #:get-parameter
   #:map-parameters
   #:send-headers
   #:headers-are-sent-p
   #:send-response
   #:query-path-sans-prefix
   #:with-request-params
   #:*request-content-length-limit*

   #:*context*
   #:context.window-component
   #:context.current-frame
   #:context.request
   #:context.response
   #:context.session
   #:context.application
   #:context.action
   #:with-dummy-context
   #:make-request-context

   #:*response*
   #:call-as-response-handler
   #:query-path
   #:network-stream
   #:html-stream
   #:response-managed-p
   #:encoding
   #:status
   #:disallow-response-caching

   #:mime-type-extension
   #:extension-mime-type
   #:mime-part-p  ; re-exported from rfc2388-binary... does this work?
   #:mime-part-headers
   #:mime-part-body

   ;; UCW URI interface
   #:uri
   #:make-uri
   #:uri.query
   #:uri.path
   #:uri.host
   #:print-uri-to-string
   #:print-uri-to-string-sans-query
   #:add-query-parameter-to-uri
   #:append-path-to-uri

   ;; Applications
   #:application
   #:application.server
   #:application.dispatchers
   #:service
   #:startup-application
   #:shutdown-application
   #:restart-application
   #:register-application
   #:unregister-application
   #:session-class
   #:*default-application*
   #:standard-application
   #:application.url-prefix
   #:basic-application

   #:defentry-point

   ;; Dispatchers
   #:dispatch
   #:dispatcher
   #:register-dispatcher
   #:action-dispatcher
   #:+action-parameter-name+
   #:url-dispatcher
   #:url-matcher
   #:url-string
   #:handler
   #:entry-point-handler
   #:handler-handle
   #:matcher
   #:matcher-match

   ;; Components
   #:defcomponent
   #:component
   #:component.calling-component
   #:*current-component*
   #:standard-component
   #:standard-component-class
   #:render-html-body
   #:call-component
   #:answer-component
   #:jump-to-component
   #:compute-url
   #:update-url
   #:find-component
   #:refresh-component
   #:parent
   #:child-components
   #:find-parent-typed
   #:component.place
   #:session-of

   ;; Windows
   #:window-component
   #:window-component.content-type

   ;; Sessions
   #:basic-session
   #:session.id
   #:find-session-id
   #:+session-parameter-name+
   #:iterate-sessions-with-lock-held
   #:mark-session-expired
   #:get-session-value
   #:delete-session
   #:notify-session-expiration
   #:ensure-session
   #:session-valid-p
   #:session-frame-class

   ;; Frames
   #:frame.id
   #:+frame-parameter-name+
   #:frame.window-component

   ;; callbacks
   #:register-callback

   ;; actions
   #:action.id
   #:+action-compound-name-delimiter+
   #:*default-action-class*
   #:make-action
   #:register-action
   #:register-action-in-frame
   #:handle-action
   #:abort-action
   #:call-action
   #:call-callbacks
   #:call-render
   #:find-action-id
   #:action-href
   #:action
   #:basic-action
   #:action-with-isolation-support
   #:action-isolated-p
   #:callback-lambda
   #:standard-session-frame
   #:*session-frame-class*
   #:register-callback-in-frame
   #:make-place
   #:place

   #:APPLICATION.CHARSET
   #:+xhtml-namespace-uri+

   ;; backend classes
   #:mod-lisp-backend
   #:multithread-mod-lisp-backend
   #:httpd-backend
   #:multithread-httpd-backend

   ;; random configuration options
   #:*inspect-components*
   #:external-format-for

   ;; rerl protocol
   #:render
   #:javascript-log-level
   #:*debug-on-error*
   #:call-render
   #:handle-toplevel-condition
   #:send-standard-error-page

   ;; backtracking
   #:backtrack
   #:backtrack-slot

   ;; cookies
   #:cookies
   #:find-cookie
   #:cookie-value
   #:add-cookie
   #:make-cookie

   ;; locks
   #:lock-of
   #:with-lock-held-on-application
   #:with-lock-held-on-session

   ;; http codes
   #:+http-continue+
   #:+http-switching-protocols+
   #:+http-ok+
   #:+http-created+
   #:+http-accepted+
   #:+http-non-authoritative-information+
   #:+http-no-content+
   #:+http-reset-content+
   #:+http-partial-content+
   #:+http-multi-status+
   #:+http-multiple-choices+
   #:+http-moved-permanently+
   #:+http-moved-temporarily+
   #:+http-see-other+
   #:+http-not-modified+
   #:+http-use-proxy+
   #:+http-temporary-redirect+
   #:+http-bad-request+
   #:+http-authorization-required+
   #:+http-payment-required+
   #:+http-forbidden+
   #:+http-not-found+
   #:+http-method-not-allowed+
   #:+http-not-acceptable+
   #:+http-proxy-authentication-required+
   #:+http-request-time-out+
   #:+http-conflict+
   #:+http-gone+
   #:+http-length-required+
   #:+http-precondition-failed+
   #:+http-request-entity-too-large+
   #:+http-request-uri-too-large+
   #:+http-unsupported-media-type+
   #:+http-requested-range-not-satisfiable+
   #:+http-expectation-failed+
   #:+http-failed-dependency+
   #:+http-internal-server-error+
   #:+http-not-implemented+
   #:+http-bad-gateway+
   #:+http-service-unavailable+
   #:+http-gateway-time-out+
   #:+http-version-not-supported+
))

;; package for automatically generated class names
(defpackage :it.bese.ucw.core.generated)
