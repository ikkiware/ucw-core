(in-package :ucw-standard)

(macrolet ((defserve ((name args &key last-modified expires
                            content-length) &body body)
             (with-unique-names (last-modified-value expires-value content-length-value if-modified-value)
               `(defun ,name ,args
                 ,(when (stringp (first body))
                        (pop body))
                 (let ((,last-modified-value ,last-modified)
                       (,content-length-value ,content-length)
                       (,expires-value ,expires)
                       (,if-modified-value (get-header request "If-Modified-Since")))
                   (setf (response-managed-p response) nil)
                   (when ,expires-value
                     (if (<= ,expires-value 0)
                         (disallow-response-caching response)
                         ;; TODO check possible timezone issues with
                         ;; calling date:universal-time-to-http-date
                         ;; like this
                         (setf (get-header response "Expires")
			       (date:universal-time-to-http-date (+ (get-universal-time)
								    ,expires-value)))))
                   (when ,last-modified-value
                     (setf (get-header response "Last-Modified")
			   (date:universal-time-to-http-date ,last-modified-value)))
                   (setf (get-header response "Date") (date:universal-time-to-http-date
                                                       (get-universal-time)))
                   (if (and ,last-modified-value
                            ,if-modified-value
                            (<= ,last-modified-value
                                (date:parse-time 
                                 ;; IE sends junk with the date (but
                                 ;; sends it after a semicolon)
                                 (subseq ,if-modified-value 0
					 (position #\; ,if-modified-value :test #'equal)))))
                       (progn
                         (ucw-log:ucw.rerl.server.dribble "defserve: Sending 304 not modified, headers only")
                         (setf (status response) +http-not-modified+
                               (get-header response "Content-Length") "0")
                         (send-headers response))
                       (progn
                         (setf (get-header response "Status") +http-ok+)
                         (when ,content-length-value
                           (setf (get-header response "Content-Length")
				 (princ-to-string ,content-length-value)))
                         ,@body)))))))
  
  (defserve (serve-stream (stream &key
                                  (request (context.request *context*))
                                  (response (context.response *context*))
                                  (last-modified (get-universal-time))
                                  content-type
                                  content-length
                                  (content-disposition "attachment" content-disposition-p)
                                  content-disposition-filename
                                  content-disposition-size
                                  (expires #.(* 24 60 60)))
                          :last-modified last-modified
                          :expires expires)
    (awhen content-type
      (setf (get-header response "Content-Type") it))
    (when (and (not content-length)
               (typep stream 'file-stream))
      (setf content-length (princ-to-string (file-length stream))))
    (unless (stringp content-length)
      (setf content-length (princ-to-string content-length)))
    (awhen content-length
      (setf (get-header response "Content-Length") it))
    (unless content-disposition-p
      ;; this ugliness here is to give a chance for the caller to pass NIL directly
      ;; to disable the default content disposition parts.
      (when (and (not content-disposition-size)
                 content-length)
        (setf content-disposition-size content-length))
      (awhen content-disposition-size
        (setf content-disposition (strcat content-disposition ";size=" it)))
      (awhen content-disposition-filename
        (setf content-disposition (strcat content-disposition ";filename=\"" it "\""))))
    (awhen content-disposition
      (setf (get-header response "Content-Disposition") it))
    (send-headers response)
    (loop
       with buffer = (make-array 8192 :element-type 'unsigned-byte)
       for end-pos = (read-sequence buffer stream)
       until (zerop end-pos) do
       (write-sequence buffer (network-stream request) :end end-pos)))

  (defserve (serve-sequence (sequence &key
                                      (request (context.request *context*))
                                      (response (context.response *context*))
                                      (last-modified (get-universal-time))
                                      (content-type "application/octet-stream")
                                      content-disposition
                                      (expires #.(* 60 60)))
                            :last-modified last-modified :expires expires)
    "Write SEQUENCE into the network stream and clean up the request. SEQUENCE
may be a string or a byte vector. When it's a string it will be encoded
according to what the (encoding response) protocol answers."
    (let* ((bytes (if (stringp sequence)
                      (string-to-octets sequence (encoding response))
                      sequence)))
      (setf (get-header response "Content-Type") content-type
            (get-header response "Content-Length") (princ-to-string (length bytes)))
      (awhen content-disposition
        (setf (get-header response "Content-Disposition") it))
      (send-headers response)
      (write-sequence bytes (network-stream request)))))

(defun serve-file (file-name &rest args &key
                   (request (context.request *context*))
                   (response (context.response *context*))
                   (last-modified nil last-modified-supplied-p)
                   (content-type nil content-type-p)
                   (content-disposition-filename nil content-disposition-filename-p)
                   content-disposition-size
                   (expires #.(* 24 60 60)))
  ;; catch file does not exist and return 404 page
  (handler-case
      (with-input-from-file (file file-name :element-type 'unsigned-byte)
	(unless last-modified-supplied-p
	  (setf last-modified (file-write-date file-name)))
	(unless content-type-p
	  (setf content-type (or content-type
				 (switch ((pathname-type file-name) :test #'string=)
				   ;; TODO it's not nice to hardcode
				   ;; utf-8 here, but file serving is
				   ;; half-assed anyway
				   ("html" "text/html; charset=UTF-8")
				   ("css"  "text/css; charset=UTF-8")
				   (t (or (extension-mime-type (pathname-type
								file-name))
					  "text/plain; charset=UTF-8"))))))
	(unless content-disposition-filename-p
	  (setf content-disposition-filename (strcat (pathname-name file-name)
						     (awhen (pathname-type file-name)
						       (strcat "." it)))))
	(apply #'serve-stream
	       file
	       :request request
	       :response response
	       :last-modified last-modified
	       :content-type content-type
	       :content-disposition-filename content-disposition-filename
	       :content-disposition-size content-disposition-size
	       :expires expires
	       args))
    (file-error (err) (send-standard-error-page :condition err
						:title "File Not Found"
						:http-status-code +http-not-found+))))