(in-package :it.bese.ucw.core)

;;;; * Core Window

(defcomponent window-component ()
  ((content-type :accessor window-component.content-type
                 :initarg :content-type
                 :initform nil ; default is text/html with charset from current application
                 :documentation "The Content-Type header for the
                 http response (also used in the meta tag)")))

(defmethod window-component.content-type :around ((window window-component))
  "Either use slot value, or compute content-type from current application charset."
  (or (call-next-method)
      (setf (window-component.content-type window)
            (format nil "text/html~@[; charset=~A~]"
                    (application.charset (context.application *context*))))))

(defmethod render :before ((window window-component))
  (setf (get-header (context.response *context*) "Content-Type")
        (window-component.content-type window)))