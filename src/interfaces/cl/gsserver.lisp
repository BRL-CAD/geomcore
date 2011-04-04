;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsserver
  (:use :cl :sb-unix)
  (:export :run :stop))

(in-package :gsserver)

(defparameter +dbdir+ "/usr/brlcad/share/db/")
(defparameter +nodename+ "Spokelse")

(defun authenticate (s user pass)
  (setf (gsnet::username s) user)
  (setf (gsnet::password s) pass)
  (and (string= user "Guest") (string= pass "Guest")))

(defun send-geom (s reuuid filename)
  (gsnet:writemsg s (make-instance 'gsnet:geommanifestmsg :manifest (list filename)))
  (with-open-file (stream (concatenate 'string +dbdir+ filename) :element-type '(unsigned-byte 8) :if-does-not-exist :error)
    (let ((arr (make-array (+ (file-length stream) 1) :element-type '(unsigned-byte 8))))
      (read-sequence arr stream)
      (gsnet:writemsg s (make-instance 'gsnet:geomchunkmsg :chunk arr :reuuid reuuid)))))

(defun send-bot-geom (s reuuid filename)
  (gsnet:writemsg s (make-instance 'gsnet:failmsg)))

(defun handle-packet (s m)
  (format t "~a~%" (type-of m))
  (cond
    ((equalp (type-of m) 'gsnet:geomreqmsg) (send-geom s (gsnet::uuid m) (gsnet::uri m)))
    ((equalp (type-of m) 'gsnet:geombotreqmsg) (send-bot-geom s (gsnet::uuid m) (gsnet::uri m)))
    ((equalp m t) m)
    ((equalp m '()) m)
    (t (format t "Unhandled thing ~a~%" (type-of m)))))

(defun handle-connection (st)
  (let ((s (make-instance 'gsnet:session :stream st)))
    
    ;;; initial handshane and authentication
    (setf (gsnet::sessionuuid s) (format '() "~a" (uuid:make-v4-uuid)))
    (setf (gsnet::localnode s) +nodename+)
    (gsnet:writemsg s (make-instance 'gsnet:nodenamemsg :name +nodename+))
    (if (not (gsnet:readmsg s)) (return-from handle-connection '()))
    (let ((m (gsnet:readmsg s)))
      (if (equalp (type-of m) 'gsnet:loginmsg)
	  (if (not (authenticate s (gsnet::username m) (gsnet::password m))) (return-from handle-connection '()))
	  (return-from handle-connection '())))
    (gsnet:writemsg s (make-instance 'gsnet::infomsg :sessionuuid (gsnet::sessionuuid s)))
    
    ;;; main loop
    (loop while (handle-packet s (gsnet:readmsg s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run (&key (listenhost #(127 0 0 1)) (port 5309))
  (usocket:socket-server listenhost port #'handle-connection  '() :element-type 'unsigned-byte :multi-threading t :in-new-thread t))

(defun stop ()
  (map 'nil (lambda (th) 
	 (cond 
	   ((equalp (sb-thread:thread-name th) "USOCKET Client") (sb-thread:terminate-thread th))
	   ((equalp (sb-thread:thread-name th) "USOCKET Server") (sb-thread:terminate-thread th))))
       (sb-thread:list-all-threads)))
