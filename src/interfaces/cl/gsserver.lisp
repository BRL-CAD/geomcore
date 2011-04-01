;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsserver
  (:use :cl :sb-unix)
  (:export :run))

(in-package :gsserver)

(defparameter +nodename+ "Spokelse")

(defun authenticate (s user pass)
  (setf (gsnet::username s) user)
  (setf (gsnet::password s) pass)
  (and (string= user "Guest") (string= pass "Guest")))

(defun send-geom (s reuuid filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8) :if-does-not-exist :error)
    (loop with a = (make-array (file-length stream) :element-type '(unsigned-byte 8))
	for i from 0 to (file-length stream)
	;;; this'd be quicker with read-sequence
	do (setf (aref a i) (read-byte stream))
	finally (writemsg s (make-instance 'geomchunkmsg :chunk a)))))

(defun handle-connection (st)
  (let ((s (make-instance 'gsnet:session :stream st)))

    ;;; initial handshane and authentication
    (setf (gsnet::localnode s) +nodename+)
    (gsnet:writemsg s (make-instance 'gsnet:nodenamemsg :name +nodename+))
    (if (not (gsnet:readmsg s)) (return-from handle-connection '()))
    (let ((m (gsnet:readmsg s)))
      (if (equalp (type-of m) 'gsnet:loginmsg)
	  (if (not (authenticate s (gsnet::username m) (gsnet::password m))) (return-from handle-connection '()))
	  (return-from handle-connection '())))
    (gsnet:writemsg s (make-instance 'gsnet::okmsg))
    
    ;;; main loop
    (loop do
	 (let ((m (gsnet:readmsg s)))
	   (cond
	     ((equalp (type-of m) 'geomreqmsg) (send-geom s (gsnet::uuid m) (gsnet::name m)))
	     ((equalp m t) '())
	     ((equalp m '()) (return-from handle-connection))
	     (t (format t "Unhandled thing ~a~%" (type-of m))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun run (&key (listenhost #(127 0 0 1)) (port 5309))
  (usocket:socket-server  listenhost port #'handle-connection  '() :element-type 'unsigned-byte)); :multi-threading t :in-new-thread t))
