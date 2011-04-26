;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsclient
  (:use :cl :sb-unix)
  (:export :login :logout :ping :getgeom :getgeomfile))

(in-package :gsclient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this should probably check to make sure things are ok
(defun getgeom (s st uri &key (bot '()))
  (gsnet:writemsg s (make-instance (if bot 'gsnet:gbrmsg 'gsnet:grmsg) :uri uri))
  (loop for i from 1 to (length (gsnet::manifest (gsnet:readmsg s))) do (let ((ch (gsnet::chunk (gsnet:readmsg s)))) (write-sequence ch st :end (- (length ch) 1)))))

(defun getgeomfile (s file uri &key (bot '()))
  (with-open-file (out file :element-type '(unsigned-byte 8) :direction :output)
    (getgeom s out uri :bot bot)))

(defun ping (s)
  (gsnet:writemsg s (make-instance 'gsnet:pingmsg))
  (let ((m (gsnet:readmsg s)))
    (- (gsnet:usec) (gsnet::tv m))))

(defun ls (s uri)
  (gsnet:writemsg s (make-instance 'gsnet:lsmsg))
  (gsnet::manifest (gsnet:readmsg s)))

; log in to a server, returning the session
(defun login (&key (username "Guest") (password "Guest") (host #(127 0 0 1)) (port 5309))
  (let ((s (make-instance 'gsnet:session :host host :port port :username username :password password)))
    (setf (gsnet::socket s) (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
    (setf (gsnet::strm s) (usocket:socket-stream (gsnet:socket s)))
    (gsnet:writemsg s (make-instance 'gsnet:rnnsetmsg :name (gsnet::localnode s)))
    (gsnet:readmsg s)
    (gsnet:writemsg s (make-instance 'gsnet:nsrmsg))
    (gsnet:readmsg s)
    s))

(defun logout (s)
  (gsnet:writemsg s (make-instance 'gsnet:drmsg))
  (usocket:socket-close (gsnet:socket s)))
