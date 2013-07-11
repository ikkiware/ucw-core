;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;; The iolib backend starts a few worker threads that are directly calling
;;; accept on the server socket. This performs better then having a central
;;; acceptor thread, because it avoids many unnecessary context switches.

(defclass iolib-backend (lockable-backend-mixin basic-backend)
  ((workers
    :initform (make-array 16 :adjustable t :fill-pointer 0)
    :accessor workers-of)
   (shutdown-initiated
    :initform nil
    :type boolean
    :accessor shutdown-initiated-p)
   (occupied-worker-count
    :initform 0
    :accessor occupied-worker-count-of)
   (maximum-worker-count
    :initform 16
    :initarg :maximum-worker-count
    :accessor maximum-worker-count-of)
   (processed-request-count
    :initform 0
    :accessor processed-request-count-of))
  (:default-initargs :host nil :port nil))

;; so we may get called with :host nil :port nil still meaning the default
(defmethod initialize-instance :after ((self iolib-backend) &key)
  (unless (host self)
    (setf (host self) iolib.sockets:+ipv4-unspecified+))
  (unless (port self)
    (setf (port self) 8080)))

(defclass iolib-message (basic-message)
  ())

(defmethod network-stream ((message iolib-message))
  (socket message))

(defmethod remote-address ((message iolib-message))
  (iolib.sockets:address-to-vector (iolib.sockets:remote-host (socket message))))

(defclass iolib-request (iolib-message basic-request)
  ())

(defclass iolib-response (iolib-message basic-response)
  ())

(defmethod make-response ((request iolib-request))
  (make-instance 'iolib-response
                 :request request
                 :socket (socket request)))

(defclass iolib-worker ()
  ((thread
    :accessor thread-of)))

(defun make-iolib-worker (backend)
  (with-lock-held-on-backend backend
    (let ((worker (make-instance 'iolib-worker)))
      (setf (thread-of worker)
            (make-thread (lambda ()
                           (iolib-worker-loop backend worker))
                         :name (format nil "iolib worker ~a" (length (workers-of backend)))))
      (vector-push-extend worker (workers-of backend))
      (ucw.backend.info "Spawned new worker thread ~A" worker)
      worker)))

(defun unregister-iolib-worker (worker backend)
  (with-lock-held-on-backend backend
    (setf (workers-of backend) (delete worker (workers-of backend)))))

(defmethod read-request ((backend iolib-backend) stream-socket)
  (let ((*request* (make-instance 'iolib-request :socket stream-socket)))
    (read-basic-request)))

(defun iolib-worker-loop (backend worker)
  (with-lock-held-on-backend backend
    ;; wait until the startup procedure finished
    )
  (unwind-protect
       (restart-case
            (iter accepting
                  (with socket = (socket backend))
                  (until (shutdown-initiated-p backend))
                  (for (values readable writable) = (iomux:wait-until-fd-ready (iolib.streams:fd-of socket) :input 1))
                  (ucw.backend.dribble "wait-until-fd-ready returned with readable ~S, writable ~S in thread ~A" readable writable (current-thread))
                  (until (shutdown-initiated-p backend))
                  (unless readable
                    (next-iteration))
                  (for stream-socket = (iolib.sockets:accept-connection socket))
                  (when stream-socket
                    (flet ((serve-one-request ()
                             (unwind-protect
                                  (progn
                                    (ucw.backend.dribble "Worker ~A is processing a request" worker)
                                    (with-lock-held-on-backend backend
                                      (incf (occupied-worker-count-of backend))
                                      (when (and (= (occupied-worker-count-of backend)
                                                    (length (workers-of backend)))
                                                 (< (length (workers-of backend))
                                                    (maximum-worker-count-of backend)))
                                        (ucw.backend.info "All ~A worker threads are occupied, starting a new one" (length (workers-of backend)))
                                        (make-iolib-worker backend))
                                      (incf (processed-request-count-of backend)))
                                    (let* ((*request* (read-request backend stream-socket))
                                           (*response* (make-response *request*)))
                                      (handle-request backend *request* *response*)
                                      (close-request *request*)))
                               (with-lock-held-on-backend backend
                                 (decf (occupied-worker-count-of backend)))))
                           (handle-request-error (condition)
                             (unless (typep condition 'no-handler-for-request)
                               (ucw.backend.error "Error while handling a iolib backend request in worker ~A on socket ~A:~%~A"
                                                  worker stream-socket condition))
                             (handle-toplevel-condition nil condition nil)
                             (ucw.backend.error "Should not get here, removing this worker...")
                             (return-from accepting)))
                      (unwind-protect
                           (progn
                             (call-as-backend-request-handler #'serve-one-request
                                                              :error-handler #'handle-request-error)
                             (ucw.backend.dribble "Worker ~A finished processing a request" worker))
                        (close stream-socket)))))
        (remove-worker ()
          :report (lambda (stream)
                    (format stream "Stop and remove worker ~A" worker))
          (values)))
    (unregister-iolib-worker worker backend)
    (ucw.backend.dribble "Worker ~A is going away" worker)))

(defmethod startup-backend ((backend iolib-backend) &key (initial-worker-count 2) &allow-other-keys)
  (assert (host backend))
  (setf (shutdown-initiated-p backend) nil)
  (with-lock-held-on-backend backend
    (loop
      (with-simple-restart (retry "Try opening the socket again on host ~S port ~S"
                                  (host backend) (port backend))
	(ucw.backend.debug "Binding socket to host ~A, port ~A" (host backend) (port backend))
        (let* ((iolib.sockets:*ipv6* nil) ; TODO: temporarily disable ipv6 because it fails
               (socket-is-ok nil)
	       (socket (iolib.sockets:make-socket :connect :passive
						:local-host (host backend)
						:local-port (port backend)
						:reuse-address t)))
          (unwind-protect
               (progn
                 (setf (iolib.streams:fd-non-blocking socket) t)
		 (ucw.backend.debug "Calling socket-listen on ~A" socket)
                 (iolib.sockets:listen-on socket)
                 ;;(iolib.sockets:set-socket-option socket :receive-timeout :sec 1 :usec 0)
                 (setf (socket backend) socket)
                 (setf socket-is-ok t)
                 (return))
            (when (and (not socket-is-ok)
                       socket)
              (close socket))))))
    (let ((ok nil))
      (unwind-protect
           (progn
             (ucw.backend.debug "Spawning the initial workers")
             (iter (for n :from 0 :below initial-worker-count)
                   (make-iolib-worker backend))
             (ucw.backend.debug "Backend successfully started")
             (setf ok t))
        (unless ok
          (ucw.backend.debug "Cleaning up after a failed backend start")
          (shutdown-backend backend :force t)))))
  backend)

(defmethod shutdown-backend ((backend iolib-backend)
                             &key force &allow-other-keys)
  (setf (shutdown-initiated-p backend) t)
  (macrolet ((kill-thread-and-catch-error (thread)
               (rebinding (thread)
                 `(block kill-worker
                   (handler-bind ((error (lambda (c)
                                           (warn "Error while killing ~S: ~A." ,thread c)
                                           (return-from kill-worker))))
                     (let ((os-thread (thread-of ,thread)))
                       (ucw.backend.dribble "Killing thread ~A; os thread is ~A" ,thread os-thread)
                       (destroy-thread os-thread)))))))
    (flet ((close-socket ()
             (awhen (socket backend)
               (setf (socket backend) nil)
               (ucw.backend.dribble "Closing socket ~A" it)
               (close it :abort force))))
      (ucw.backend.dribble "Shutting down iolib-backend ~A, force? ~A" backend force)
      (if force
          (progn
            (iter (for worker :in-sequence (copy-seq (workers-of backend)))
                  (kill-thread-and-catch-error worker))
            (close-socket)
            (setf (fill-pointer (workers-of backend)) 0)
            (setf (occupied-worker-count-of backend) 0))
          (progn
            (iter waiting-for-workers
              (ucw.backend.debug "Waiting for the workers of ~A to quit..." backend)
              (with-lock-held-on-backend backend
                (when (zerop (length (workers-of backend)))
                  (return-from waiting-for-workers)))
              (sleep 1))
            (assert (zerop (occupied-worker-count-of backend)))
            (close-socket))))))

;; Copyright (c) 2006-2007 Attila Lendvai
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
