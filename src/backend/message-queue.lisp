;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;; TODO the messaging stuff should be part of another lib, probably bordeaux-threads.

;;;
;;; A simple message queue implementation
;;;
(defclass message-queue ()
  ((name
    :initform "A message queue"
    :accessor name-of
    :initarg :name)
   (lock
    :initform (make-lock "message queue")
    :accessor lock-of)
   (messages
    :initform '()
    :accessor messages-of)
   (message-received-condition
    :initform (make-condition-variable)
    :accessor message-received-condition-of)))


(defmethod initialize-instance :after ((self message-queue) &rest args)
  (setf (lock-of self) (make-lock (format nil "Lock for ~S" (name-of self)))))

(defun make-message-queue (&rest initargs)
  (apply #'make-instance 'message-queue initargs))

(defmacro with-lock-held-on-message-queue (queue &body body)
  (rebinding (queue)
    `(with-lock-held ((lock-of ,queue))
      (ucw.backend.dribble "Entering with-lock-held-on-message-queue for queue ~A in thread ~A" ,queue (current-thread))
      (multiple-value-prog1
          ,@body
        (ucw.backend.dribble "Leaving with-lock-held-on-message-queue for queue ~A in thread ~A" ,queue (current-thread))))))

(defgeneric receive-message (message-queue))

(defgeneric send-message (message message-queue))

(defmethod receive-message ((queue message-queue))
  (ucw.backend.dribble "Receiving message from queue ~A in thread ~A" queue (current-thread))
  (with-lock-held-on-message-queue queue
    (multiple-value-prog1
        (loop
          (if (messages-of queue)
              (return (pop (messages-of queue)))
              (progn
                (ucw.backend.dribble "No message in queue ~A, condition-wait in thread ~A" queue (current-thread))
                (condition-wait (message-received-condition-of queue)
                                (lock-of queue)))))
      (ucw.backend.dribble "Returning a received message from queue ~A in thread ~A" queue (current-thread)))))

(defmethod send-message (message (queue message-queue))
  (ucw.backend.dribble "Sending message to queue ~A from thread ~A" queue (current-thread))
  (with-lock-held-on-message-queue queue
    (setf (messages-of queue) (nconc (messages-of queue) (list message)))
    (ucw.backend.dribble "Notifying message-received-condition of queue ~A from thread ~A" queue (current-thread))
    (condition-notify (message-received-condition-of queue)))
  (ucw.backend.dribble "Done sending message to queue ~A from thread ~A" queue (current-thread))
  (values))

(defmacro send (thread &rest message-contents)
  (rebinding (thread)
    (with-unique-names (message)
      `(let ((,message (list ,@message-contents)))
         (ucw.backend.dribble "Sending ~S to ~S from thread ~A" ,message ,thread (current-thread))
         (send-message ,message ,thread)))))

(defmacro receive (thread &body message-match-clauses)
  (with-unique-names (message)
    (rebinding (thread)
      `(let ((,message (receive-message ,thread)))
        (ucw.backend.dribble "~S received ~S" ,thread ,message)
        (list-match-case ,message
          ,@message-match-clauses
          (?_
           (error "Unknown message ~S received by ~S" ?_ ,thread)))))))

(defclass thread-with-message-queue-mixin ()
  ((message-queue
    :initform (make-message-queue)
    :accessor message-queue-of)
   (thread
    :accessor thread-of
    :initarg :thread)))

(defprint-object (self thread-with-message-queue-mixin)
  (write-string (thread-name (thread-of self))))

(defmethod send-message (message (self thread-with-message-queue-mixin))
  (send-message message (message-queue-of self)))

(defmethod receive-message ((self thread-with-message-queue-mixin))
  (receive-message (message-queue-of self)))

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
