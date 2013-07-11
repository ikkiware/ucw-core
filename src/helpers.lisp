;;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; * Miscalaneous helper code

;; TODO copied from alexandria. use alexandria eventually...
(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

(defun system-relative-pathname (system path)
  "Return a pathname relative to the given asdf system."
  (merge-pathnames path (asdf:component-pathname (asdf:find-system system))))

(defun filename-for-temporary-file (&optional (prefix "ucw"))
  (strcat *directory-for-temporary-files*
	  prefix
	  (princ-to-string (mod (get-internal-real-time) 100000))))

(defun open-temporary-file (&rest args &key
                            (element-type '(unsigned-byte 8))
                            (direction :output))
  (iter
    (for file-name = (filename-for-temporary-file))
    (for file = (apply #'open
                       file-name
                       :if-exists nil
                       :direction direction
                       :element-type element-type
                       args))
    (until file)
    (finally (return (values file file-name)))))

(defun new-random-key (hash-table key-length)
  (iter (for key = (random-string key-length))
        (for (values value foundp) = (gethash key hash-table))
        (while foundp)
        (finally (return key))))

(defun insert-with-new-key (hash-table key-length value)
  "helper method. generates random strings of length key-length until
  it finds one that isn't a key in hash-table and sets value to
  that. returns the new id."
  (let ((key (new-random-key hash-table key-length)))
    (setf (gethash key hash-table) value)
    key))

(defstruct (shared-hashtable-entry (:conc-name shsh-))
  (table nil)
  (lock nil)
  (last-purge-time nil :type (or null integer))
  (access-count-until-purge nil :type (or null integer)))

(defmacro define-shared-hashtable (name &rest args &key purge-interval-secs purge-interval-access
                                        purge-interval-size &allow-other-keys)
  (let ((entry-name (intern-concat (list "*" name "*")))
        (with-lock-held-name (intern-concat (list "WITH-LOCK-HELD-ON-" name))))
    (remf-keywords args :purge-interval-size :purge-interval-secs :purge-interval-access)
    `(progn
      (defparameter ,entry-name
        (make-shared-hashtable-entry :table (make-hash-table ,@args)
                                     :lock (make-lock ,(string-downcase (string entry-name)))
                                     :last-purge-time (get-universal-time)
                                     :access-count-until-purge ,purge-interval-access))
      (defmacro ,with-lock-held-name (&body body)
        `(with-lock-held ((shsh-lock ,',entry-name))
          ,@body))
      (defmacro ,(intern-concat (list "ENSURE-" name "-VALUE")) (key &body body)
        (with-unique-names (now entry value table)
          (declare (ignorable now))
          (rebinding (key)
            ;; TODO we should use a read-write lock here
            `(,',with-lock-held-name
              (let* ((,entry ,',entry-name)
                     (,table (shsh-table ,entry))
                     (,value (gethash ,key ,table)))
                ,,(when purge-interval-size
                        ``(when (> (hash-table-count (shsh-table ,entry)) ,',purge-interval-size)
                           (clrhash ,table)))
                ,,(when purge-interval-secs
                        ``(let ((,now (get-universal-time)))
                           (when (> (+ ,',purge-interval-secs (shsh-last-purge-time ,entry)) now)
                             (setf (shsh-last-purge-time ,entry) now)
                             (clrhash ,table))))
                ,,(when purge-interval-access
                        ``(when (minusp (decf (shsh-access-count-until-purge ,entry)))
                           (setf (shsh-access-count-until-purge ,entry) ,',purge-interval-access)
                           (clrhash ,table)))
                (unless ,value
                  (setf ,value (progn ,@body))
                  (when ,value
                    (setf (gethash ,key ,table) ,value)))
                ,value))))))))

#+sbcl
(defmacro with-thread-name (name &body body)
  (with-unique-names (thread previous-name)
    `(let* ((,thread sb-thread:*current-thread*)
            (,previous-name (sb-thread:thread-name ,thread)))
       (setf (sb-thread:thread-name ,thread)
             (concatenate 'string ,previous-name ,name))
       (unwind-protect
            (progn
              ,@body)
         (setf (sb-thread:thread-name ,thread) ,previous-name)))))

#-sbcl
(defmacro with-thread-name (name &body body)
  (declare (ignore name))
  `(progn
     ,@body))


;;;; ** Simple URL manipulation

;;;; May be replaced with something better (puri?) should the need
;;;; arise.

;; TODO consider using puri.
;; puri:parse-uri fails with escaped non-ascii (utf-8) chars:
;; (puri::decode-escaped-encoding "arg1=%C3%A9%C3%A1%C3%B3%C3%BC%C3%B6" t)
(defclass uri ()
  ((scheme   :initarg :scheme   :initform nil :accessor uri.scheme)
   (host     :initarg :host     :initform nil :accessor uri.host)
   (port     :initarg :port     :initform nil :accessor uri.port)
   (path     :initarg :path     :initform nil :accessor uri.path)
   (query    :initarg :query    :initform nil :accessor uri.query)
   (fragment :initarg :fragment :initform nil :accessor uri.fragment)))

(defprint-object (uri uri :identity nil)
  (write-uri uri *standard-output* nil))

(defun make-uri (&rest initargs)
  (apply #'make-instance 'uri initargs))

(defun add-query-parameter-to-uri (uri name value)
  (setf (uri.query uri)
        (nconc (uri.query uri) (list (cons name value)))))

(defun append-path-to-uri (uri path)
  (setf (uri.path uri)
        (strcat (uri.path uri) path)))

(defun write-uri-sans-query (uri stream &optional (escape t))
  "Write URI to STREAM, only write scheme, host and path."
  (declare (optimize (speed 3)))
  (with-accessors ((scheme uri.scheme) (host uri.host)
                   (port uri.port) (path uri.path))
       uri
    (flet ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
      (when scheme
        (out scheme)
        (write-string "://" stream))
      (when host
        (out host)
        (when port
          (write-string ":" stream)
          (princ port stream)))
      (out path))))

(defun write-uri (uri stream &optional (escape t))
  (declare (optimize (speed 3)))
  (with-accessors ((query uri.query) (fragment uri.fragment)) uri
    (write-uri-sans-query uri stream escape)
    (labels ((out (string)
               (funcall (if escape
                            #'write-as-uri
                            #'write-string)
                        string stream))
             (write-query-part (name value)
               (out name)
               (write-char #\= stream)
               (out (typecase value
                      (number (princ-to-string value))
                      (t (string value))))))
      (when query
        (iter (for (name . value) in query)
              (write-char (if (first-iteration-p) #\? #\&) stream)
              (write-query-part name value)))
      (when fragment
        (write-char #\# stream)
        (out fragment)))))

(defun print-uri-to-string (uri &optional (escape t))
  (with-output-to-string (string)
    (write-uri uri string escape)))

(defun print-uri-to-string-sans-query (uri)
  (with-output-to-string (string)
    (write-uri-sans-query uri string)))

;; TODO consider depending on iolib, it has all the required infrastructure
(deftype ip-v4-address ()
  `(simple-array (unsigned-byte 8) (4)))

(deftype ip-v6-address ()
  `(simple-array (unsigned-byte 16) (8)))

(deftype ip-address ()
  `(or ip-v4-address ip-v6-address))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ip-address= (a b)
    (declare (type ip-address a b))
    (equalp a b))

(defvar  +ip-v4-localhost+ (coerce #(127 0 0 1) 'ip-v4-address))
(defvar  +ip-v6-localhost+ (coerce #(0 0 0 0 0 0 0 1) 'ip-v6-address)))



(defun ip-address-from-private-network-p (address)
  "http://en.wikipedia.org/wiki/Private_network"
  (declare (optimize speed)
           (type ip-address address))
  (and (typep address 'ip-v4-address)
       (let ((first (aref address 0))
             (second (aref address 1)))
         (or (= first 10)
             (and (= first 172)
                  (<= 16 second 31))
             (and (= first 192)
                  (= second 168))))))

(defun localhost-ip-address-p (address)
  (declare (type ip-address address))
  (etypecase address
    (ip-v4-address (ip-address= address #.+ip-v4-localhost+))
    (ip-v6-address (ip-address= address #.+ip-v6-localhost+))))

;; Copyright (c) 2008 Drew Crampsie
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
