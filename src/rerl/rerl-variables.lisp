;; -*- lisp -*-

(in-package :it.bese.ucw.core)

;;;; ** Standard RERL configuration variables

(defvar *current-form*)

;; TODO create two specials: *maximum-session-longevity* and *minimum-session-longevity*
;; TODO duplicate most of these as slots on applications taking the default from the var's
(defvar *default-session-longevity* (* 60 30)
  "Seconds of inactivity allowed before a basic-session object is expired.")

(defvar *default-session-purge-period* (* 60)
  "Seconds of minimal delay between looking for expired sessions.")

(defvar *maximum-number-of-sessions* 10000
  "UCW will render a '503 Service Unavailable' when there are this many live sessions in an application and new ones are coming.")

(defconstant +session-id-length+ 40
  "Length, in chars, of the automatically generated session ids.")

(defvar +session-parameter-name+ "_s")

(defvar +session-parameter-keyword+ :_s)

(defvar +session-backtracking-max-depth+ 32
  "How many frames we keep in backtracking history.")

(defconstant +frame-id-length+ 20
  "Length, in chars, of the automatically generated frame ids.")

(defvar +frame-parameter-name+ "_f")

(defvar +frame-parameter-keyword+ :_f)

(defconstant +action-id-length+ 10
  "Length, in chars, of the automatically generates action ids.")

(defvar +action-parameter-name+ "_a")

(defvar +action-invocation-parameter-name+ "_i"
  "This parameter identifies an invocation of an action (to be used when the action is invocation-isolated-p)")

(defvar +action-parameter-keyword+ :_a)

(defconstant +callback-id-length+ 5)

(eval-always
  (defparameter +ucw-dynamic-url-prefix+ "dynamic/"))

(defun map-to-dynamic-ucw-url (url)
  "Prefixes random UCW internal url's to ensure that it does not clash with some user defined entrypoint."
  (concatenate 'string #.+ucw-dynamic-url-prefix+ url))

(defparameter +i18n-parenscript-dispatcher-url+ (map-to-dynamic-ucw-url "js/per-locale.js"))
(defparameter +polling-dispatcher-url+          (map-to-dynamic-ucw-url "polling"))
(defparameter +ajax-action-dispatcher-url+      (map-to-dynamic-ucw-url "ajax"))
(defparameter +callback-dispatcher-url+         (map-to-dynamic-ucw-url "callback"))

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
