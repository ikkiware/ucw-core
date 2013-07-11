;;; -*- lisp -*-

(in-package :it.bese.ucw.loggers)

;;;; * The Standard Loggers

(defmacro defucwlogger (name parents &rest options)
  `(progn (deflogger ,name ,parents ,@options)
	  (export '(,name
		    ,@(mapcar (lambda (class)
				(intern (strcat (symbol-name name)
						"."
						(symbol-name class))
					:it.bese.ucw.loggers))
			      '(#:dribble #:debug #:info #:warn #:error #:fatal))))))

(defucwlogger ucw ()
  :level *ucw-log-level*
  :compile-time-level *ucw-compile-time-log-level*
  :appender (make-instance 'arnesi:brief-stream-log-appender :stream *debug-io*))

(defucwlogger ucw.rerl (ucw))
(defucwlogger ucw.rerl.actions (ucw.rerl))
(defucwlogger ucw.rerl.threads (ucw.rerl))
(defucwlogger ucw.rerl.server (ucw.rerl))
(defucwlogger ucw.rerl.application (ucw.rerl))
(defucwlogger ucw.rerl.session (ucw.rerl))
(defucwlogger ucw.rerl.session-frame (ucw.rerl))
(defucwlogger ucw.rerl.dispatcher (ucw.rerl))
(defucwlogger ucw.rerl.ajax (ucw.rerl))

(defucwlogger ucw.l10n (ucw))

(defucwlogger ucw.component (ucw.rerl))
(defucwlogger ucw.component.dojo (ucw.component))
(defucwlogger ucw.component.render (ucw.component))
(defucwlogger ucw.component.action (ucw.component))
(defucwlogger ucw.component.layers (ucw.component))

(defucwlogger ucw.backend (ucw))

(defucwlogger ucw.admin (ucw))

(defucwlogger ucw.examples (ucw))

(defun ucw.log-level ()
  (log.level (get-logger 'ucw)))

(defun (setf ucw.log-level) (level)
  (setf (log.level (get-logger 'ucw)) level))

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

