(in-package #:common-lisp-user)

(defpackage #:ucw-standard
  (:nicknames :ucw)
  (:use :ucw-core :cl :arnesi :iterate :yaclml)
  (:shadowing-import-from :ucw-core :parent)
  (:export

   ;; Actions
   #:answer
   #:call
   #:call-as-window
   #:jump
   #:defaction
   #:*default-action-class*
   #:*source-component*
   #:standard-action
   #:redirect-to

   ;; Applications
   #:standard-application
   ;; www-root application
   #:static-roots-application-mixin
   #:application.static-roots
   ;; cookie session application
   #:cookie-session-application-mixin
   #:cookie-session-request-context
   ;; secure application
   #:secure-application-mixin
   #:secure-application-p
   #:application-find-user
   #:application-check-password
   #:application-authorize-call
   #:on-authorization-reject
   #:session-user
   #:session-authenticated-p
   #:user-login
   #:login-user
   #:logout-user
   #:exit-user
   ;; template application
   #:tal-application-mixin
   ;; user tracking application
   #:user-track-application-mixin
   #:application.online-users

   ;; Cached output
   #:cache-component-mixin
   #:component-cache
   #:component-cache-invalid-p
   #:note-cache-updated
   #:num-hits-cache-component-mixin
   #:timeout-cache-component-mixin

   ;; Containers
   #:add-component
   #:child-components
   #:clear-container
   #:component-at
   #:container
   #:container.contents
   #:container.current-component
   #:container.current-component-key
   #:container.key-generator
   #:container.key-test
   #:find-component
   #:list-container
   #:remove-component
   #:switching-container
   #:switch-component
   #:tabbed-pane
   #:render-pane-contents
   #:render-pane-options

   ;; Dispatchers
   #:minimal-dispatcher
   #:regexp-dispatcher
   #:starts-with-dispatcher
   #:simple-dispatcher
   #:*dispatcher-registers*
   #:*dispatcher-url-suffix*

   ;; Error messages
   #:error-message-window

   ;; File/Sequence Serving
   #:serve-sequence
   #:serve-stream
   #:serve-file

   ;; HTML Elements
   #:html-block-element-mixin
   #:html-element-mixin
   #:html-element.css-class
   #:html-element.css-style
   #:html-element.dom-id
   #:html-inline-element-mixin
   #:unique-dom-id

   ;; Option Dialog
   #:option-dialog
   #:respond

   ;; Paged List Component
   #:paged-list
   #:paged-list.have-next-p
   #:paged-list.have-previous-p
   #:paged-list.current-page
   #:paged-list.offset
   #:paged-list.page-count
   #:paged-list.page-size
   #:paged-list.pages
   #:render-paged-list-item
   #:scroll-end
   #:scroll-backward
   #:scroll-forward
   #:scroll-start
   #:scroll-to-page
   
   ;; User login
   #:user-login
   #:user-login-window
   #:cancel
   #:submit

   ;; Windows
   #:basic-windows-features-mixin
   #:standard-window-component
   #:window-component
   #:window-component.content-prologue
   #:window-component.content-type
   #:window-component.doctype
   #:window-component.html-tag-attributes
   #:window-component.icon
   #:window-component.javascript
   #:window-component.stylesheet
   #:window-component.title
   #:window-body

   ;; TAL
   #:tal-component
   #:simple-tal-component
   #:render-template
   #:tal-component-environment

   ;; Task
   #:task-component
   #:start

   ;; Transaction
   #:transactional-application-mixin
   #:open-transaction
   #:close-transaction))


(defpackage #:ucw-tags
  (:use #:yaclml :cl)
  (:nicknames #:<UCW)
  (:export 
   #:a
   #:form
   #:input
   #:submit
   #:select
   #:option
   #:textarea
   ;; TAL
   #:render-component
   #:render-component-body))

(defpackage #:ucw-user
  (:shadowing-import-from #:ucw-core #:parent)
  (:use
   :common-lisp :ucw-standard :ucw-core :arnesi)
  (:export #:*example-server*))


