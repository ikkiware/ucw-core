;;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; Library of functions shared by multiple backends.

(defconstant +Space+ #.(char-code #\Space))
(defconstant +Tab+ #.(char-code #\Tab))
(defconstant +Colon+ #.(char-code #\:))
(defconstant +Linefeed+ 10)
(defconstant +Carriage-Return+ 13)

(defun split-on-space (line)
  "Split line on #\Space."
  (iter outer ; we only need the outer to be able to collect a last one in the finally of the inner
        (iter (with start = 0)
              (for end upfrom 0)
              (for char in-vector line)
              (when (= +Space+ char)
                (in outer (collect (make-displaced-array line start end)))
                (setf start (1+ end)))
              (finally (in outer (collect (make-displaced-array line start)))))
        (while nil)))

(defun split-header-line (line)
  (declare (type (vector (unsigned-byte 8)) line))
  (let* ((colon-position (position +Colon+ line :test #'=))
         (name-length colon-position)
         (value-start (1+ colon-position))
         (value-end (length line)))
    ;; skip any leading space char in the header value
    (iterate
      (for start upfrom value-start)
      (while (< start value-end))
      (for byte = (aref line start))
      (while (or (= +Space+ byte)
                 (= +Tab+ byte)))
      (incf start)
      (finally (setf value-start (1- start))))
    (cons (make-displaced-array line 0 name-length)
          (make-displaced-array line value-start value-end))))

(defun make-cookie (name value &rest initargs &key (path nil path-p) &allow-other-keys)
  (apply #'rfc2109:make-cookie
         :name name
         :value (escape-as-uri value)
         (if path-p
             (list* :path (escape-as-uri path) initargs)
             initargs)))

;;;; servinge static files

(defvar *mime-types* nil)

(defvar *default-mime-types-file* #P"/etc/mime.types")

(defun parse-mime-types (mime-types-file)
  "Parse mime.types file"
  (iterate
    (for line in-file mime-types-file using #'read-line)
    (when (or (string= "" line)
              (eq #\# (aref line 0)))
      (next-iteration))
    (for split = (cl-ppcre:split "( |	)" line))
    (unless (null split)
      (collect (iter (for element :in split)
                     (unless (zerop (length element))
                       (collect element)))))))

(defun read-mime-types (mime-types-file)
  "Read in mime.types file."
  (iterate
    (for (type . extensions)
          in (parse-mime-types mime-types-file))
    (aif (assoc type *mime-types* :test #'string-equal)
         (setf (cdr it) (nconc (cdr it) extensions))
         (push (cons type extensions) *mime-types*))))

(defun mime-type-extensions (type)
  "Extensions that can be given to file of given MIME type."
  (cdr (assoc type *mime-types* :test #'string-equal)))

(defun extension-mime-type (extension)
  "MIME type associated with file extension."
  (first
   (find-if #'(lambda (typespec)
                (find extension (rest typespec)
                      :test #'string-equal))
            *mime-types*)))

(defun map-query-path-to-file (query-path url-base directory)
  "Converts QUERY-PATH to a file on the local filesystem assuming
  the application is mapped to URL-BASE and lives in DIRECTORY.

In other words: if query-path is \"<URL-BASE><SOMETHING>\"
returns: \"<DIRECTORY><SOMETHING>\" (assuming said file
exists), otherwise returns NIL."
  ;; NB: We really could (and should) cache this function on
  ;; QUERY-PATH, but we need to figure out how to wipe the cache when
  ;; an application's url-prefix changes. considering how rarely that
  ;; happens it's sucks that we lose the speed increase this could
  ;; bring...
  (multiple-value-bind (starts-with <something>)
      (starts-with query-path url-base :return-suffix t)
    (when starts-with
      (let* ((pathname (merge-pathnames <something> directory))
             (truename (ignore-errors
                         (probe-file pathname))))
        (when (and truename
                   (not (cl-fad:directory-pathname-p truename)))
          (return-from map-query-path-to-file truename))))
    nil))

(defun disallow-response-caching (response)
  "Sets the appropiate response headers that will instruct the clients not to cache this response."
  (setf (get-header response "Expires") #.(date:universal-time-to-http-date +epoch-start+)
        (get-header response "Cache-Control") "no-store"
        (get-header response "Pragma") "no-cache"))

;;;; Parsing HTTP request bodies.

;;;; The httpd and mod_lisp backends use this code.

(defun grab-param (param-string start =-pos end)
  "Returns (KEY . VALUE) of the request param whose first char
  is at START, whose \#= char is at =-POS and whose last char is
  at (1+ END.).

  =-POS may be NIL, END may be equal to =-POS or the last index
  of START."
  (let* ( ;; the index of the first char of the key
         (key-start start)
         ;; the index of the char immediatly after the last char of key
         (key-end (or =-pos end))
         (key (make-displaced-array param-string key-start key-end))
         ;; the index of the first char of the value
         (value-start (if =-pos
                          (1+ =-pos)
                          end))
         ;; the index of the char immediatly after the
         ;; end of the value (may be equal to
         ;; key-start in the case of "" values).
         (value-end end)
         (value (if value-end
                    (make-displaced-array param-string value-start value-end)
                    ""))
         ;; TODO can we use nunescape-as-uri here? if we rename to nparse-query-parameters?
         (unescaped-key (unescape-as-uri key))
         (unescaped-value (unescape-as-uri value)))
    (ucw.backend.dribble "Grabbed parameter ~S with value ~S." unescaped-key unescaped-value)
    (cons unescaped-key unescaped-value)))

(defun parse-query-parameters (param-string)
  (let ((params '()))
    (when (and param-string (< 0 (length param-string)))
      (iterate
        (with start = 0)
        (with =-pos = nil)
        (for char in-vector param-string)
        (for offset upfrom 0)
        (case char
          (#\& ;; end of the current param
           (push (grab-param param-string start =-pos offset) params)
           (setf start (1+ offset)
                 =-pos nil))
          (#\= ;; end of name
           (setf =-pos offset)))
        ;; automatic end of param string
        (finally (push (grab-param param-string start =-pos (1+ offset)) params))))
    (nreverse params)))

(defun rfc2388-callback (mime-part)
  (declare (optimize speed))
  (ucw.backend.dribble "Processing mime part ~S." mime-part)
  (let* ((header (rfc2388-binary:get-header mime-part "Content-Disposition"))
         (disposition (rfc2388-binary:header-value header))
         (name (rfc2388-binary:get-header-attribute header "name"))
         (filename (rfc2388-binary:get-header-attribute header "filename")))
    (ucw.backend.dribble "Got a mime part. Disposition: ~S; Name: ~S; Filename: ~S" disposition name filename)
    (ucw.backend.dribble "Mime Part: ---~S---~%" (with-output-to-string (dump)
                                                   (rfc2388-binary:print-mime-part mime-part dump)))
    (cond
      ((or (string-equal "file" disposition)
           (not (null filename)))
       (multiple-value-bind (file tmp-filename)
           (open-temporary-file)
         (setf (rfc2388-binary:content mime-part) file)
         (ucw.backend.dribble "Sending mime part data to file ~S (~S)."
                              tmp-filename (rfc2388-binary:content mime-part))
         (let* ((counter 0)
                (buffer (make-array 8196 :element-type '(unsigned-byte 8)))
                (buffer-length (length buffer))
                (buffer-index 0))
           (declare (type array-index buffer-length buffer-index counter))
           (values (lambda (byte)
                     (declare (type (unsigned-byte 8) byte))
                     (ucw.backend.dribble "File byte ~4,'0D: ~D~:[~; (~C)~]"
                                          counter byte (<= 32 byte 127)
                                          (code-char byte))
                     (setf (aref buffer buffer-index) byte)
                     (incf counter)
                     (incf buffer-index)
                     (when (>= buffer-index buffer-length)
                       (write-sequence buffer file)
                       (setf buffer-index 0)))
                   (lambda ()
                     (ucw.backend.dribble "Done with file ~S." (rfc2388-binary:content mime-part))
                     (unless (zerop buffer-index)
                       (write-sequence buffer file :end buffer-index))
                     (ucw.backend.dribble "Closing ~S." (rfc2388-binary:content mime-part))
                     (close file)
                     (ucw.backend.dribble "Closed, repoening.")
                     (setf (rfc2388-binary:content mime-part)
                           (open tmp-filename
                                 :direction :input
                                 :element-type '(unsigned-byte 8)))
                     (ucw.backend.dribble "Opened ~S." (rfc2388-binary:content mime-part))
                     (cons name mime-part))
                   (lambda ()
                     (close file)
                     (delete-file tmp-filename))))))
      ((string-equal "form-data" disposition)
       (ucw.backend.dribble "Grabbing mime-part data as string.")
       (setf (rfc2388-binary:content mime-part) (make-array 10
                                                            :element-type '(unsigned-byte 8)
                                                            :adjustable t
                                                            :fill-pointer 0))
       (let ((counter 0))
         (declare (type array-index counter))
         (values (lambda (byte)
                   (declare (type (unsigned-byte 8) byte))
                   (ucw.backend.dribble "Form-data byte ~4,'0D: ~D~:[~; (~C)~]."
                                        counter byte (<= 32 byte 127)
                                        (code-char byte))
                   (incf counter)
                   (vector-push-extend byte (rfc2388-binary:content mime-part)))
                 (lambda ()
                   (let ((content (octets-to-string (rfc2388-binary:content mime-part)
                                                    (or (external-format-for :url) :us-ascii))))
                     (ucw.backend.dribble "Done with form-data ~S: ~S" name content)
                     (cons name content))))))
      (t
       (error "Don't know how to handle the mime-part ~S (disposition: ~S)"
              mime-part header)))))

(defun parse-request-body (stream raw-content-length raw-content-type)
  (when (and raw-content-length
             raw-content-type)
    (with-thread-name " / PARSE-REQUEST-BODY"
      (let ((content-length (parse-integer raw-content-length :junk-allowed t)))
        (unless (or (not content-length)
                    (<= content-length 0))
          (multiple-value-bind (content-type attributes) (rfc2388-binary:parse-header-value raw-content-type)
            (switch (content-type :test #'string=)
              ("application/x-www-form-urlencoded"
               (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
                 (read-sequence buffer stream)
                 (return-from parse-request-body
                   (parse-query-parameters
                    (aif (cdr (assoc "charset" attributes :test #'string=))
                         (eswitch (it :test #'string=)
                           ("ASCII" (octets-to-string buffer :us-ascii))
                           ("UTF-8" (octets-to-string buffer :utf-8)))
                         (octets-to-string buffer :iso-8859-1))))))
              ("multipart/form-data"
               (when (and *request-content-length-limit*  (> content-length *request-content-length-limit*))
                 (request-content-length-limit-reached content-length))
               (let ((boundary (cdr (assoc "boundary" attributes :test #'string=))))
                 ;; TODO DOS prevention: add support for rfc2388-binary to limit parsing length if the ContentLength header is fake, pass in *request-content-length-limit*
                 (return-from parse-request-body
                   (rfc2388-binary:read-mime stream boundary #'rfc2388-callback))))
              (t (abort-backend-request "Invalid request content type"))))))))
  (ucw.backend.debug "Skipped parsing request body, raw Content-Type is [~S], raw Content-Length is [~S]"
                     raw-content-type raw-content-length)
  (list))

(defmethod mime-part-body ((mime-part rfc2388-binary:mime-part))
  (rfc2388-binary:content mime-part))

(defmethod mime-part-headers ((mime-part rfc2388-binary:mime-part))
  (mapcar (lambda (header)
            (cons (rfc2388-binary:header-name header)
                  (rfc2388-binary:header-value header)))
          (rfc2388-binary:headers mime-part)))

(defmethod encoding ((response response))
  (or (awhen (and (boundp '*context*)
                  (context.application *context*))
        (application.charset it))
      (external-format-for :http)
      :iso-8859-1))

(defun call-as-backend-request-handler (thunk &key (error-handler 'abort-backend-request))
  (restart-case
       (let ((swank::*sldb-quit-restart* 'abort-backend-request))
         (call-with-ucw-error-handler
          thunk error-handler))
    (abort-backend-request ()
      :report "Abort processing this request at the backend level"
      (values))))

(defun abort-backend-request (&optional (why nil why-p))
  (ucw.backend.info "Gracefully aborting backend request~:[.~; because: ~A.~]" why-p why)
  (invoke-restart (find-restart 'abort-backend-request)))

;; Copyright (c) 2003-2006 Edward Marco Baringer
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
