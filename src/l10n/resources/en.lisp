(in-package :lang)

(defresources en
  (file-last-modification-timestamp<> (file)
    (<:as-html "Updated: ")
    (aif (file-write-date file)
         (timestamp<> (local-time:local-time :universal it))
         (<:span :class "missing-file"
                 "File is missing!")))
  (your-session-has-expired "Your session has expired or has been deleted on the server"))

(define-js-resources en
  (confirm-pending-changes
   #.(format nil "You have pending changes.~%Are you sure you want to abandon them?"))

  (warning.session-will-time-out-at "Your session will time out at '${deadline}'!")
  (warning.session-timed-out "Your session had timed out, connection is lost!")

  (unknown-error-while-processing-server-answer
   "Error while processing the request. Try to reload the page and if the error persists then contact the technical support.")

  (ucw.session-expired-approve-redirect-message
   "Your session has expired. Do you want to reload the page to start a fresh one?")
  (unknown-server-error "Unknown server error")
  (network-error "There was an error while communicating with the server, try again.")
  
  (progress.tooltip "Click to remove")
  (progress-label.default "Loading...")
  (progress-label.closing-tab "Closing tab...")
  (progress-label.loading-tab "Loading tab...")
  (progress-label.loading-container-child "Loading..."))
