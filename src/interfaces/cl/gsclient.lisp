;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsclient
  (:use :cl :sb-unix)
  (:export :login :logout :ping :getgeom))

(in-package :gsclient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getgeom (s st uri)
  (gsnet:writemsg s (make-instance 'geomreqmsg :uri uri))
  (let ((mfst (gsnet:readmsg s)))
    (loop for i from 0 to (length (gsnet:manifest mfst)) do
	 (let ((cm (chunk (gsnet:readmsg (strm s)))))
	   (loop for j from 0 to (length cm) do (write-byte (aref cm j) st))))))

(defun ping (s)
  (gsnet:writemsg s (make-instance 'gsnet:pingmsg))
  (let ((m (gsnet:readmsg s)))
    (format t "response holds: ~a~%" (gsnet::tv m))
    (- (gsnet:usec) (gsnet::tv m))))

; log in to a server, returning the session
(defun login (&key (username "Guest") (password "Guest") (host #(127 0 0 1)) (port 5309))
  (let ((s (make-instance 'gsnet:session :host host :port port :username username :password password)))
    (setf (gsnet::socket s) (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
    (setf (gsnet::strm s) (usocket:socket-stream (gsnet:socket s)))
    (gsnet:readmsg s)
    (gsnet:writemsg s (make-instance 'gsnet:nodenamemsg :name (gsnet::localnode s)))
    (format t "Remote name: ~a~%" (gsnet:remotenode s))
    (gsnet:writemsg s (make-instance 'gsnet:loginmsg))
    (gsnet:readmsg s)
    (format t "Session UUID: ~a~%" (gsnet:sessionuuid s))
    s))

(defun logout (s)
  (gsnet:writemsg s (make-instance 'gsnet:logoutmsg))
  (usocket:socket-close (gsnet:socket s)))
