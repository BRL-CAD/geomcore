;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsserver
  (:use :cl)
  (:export :run :stop))

(in-package :gsserver)

(defvar *dbdir* "GS_repository")
(defvar *nodename* "Spokelse")
(defvar *verbose* t)

(defun authenticate (s user pass)
  (setf (gsnet::username s) user)
  (setf (gsnet::password s) pass)
  (and (string= user "Guest") (string= pass "Guest")))

(defun send-geom (s reuuid filename)
  (handler-case
      (let ((repo (gvm:gvm-open *dbdir*))
	    (tmpnam (sb-posix:mktemp "cltmp.XXXXX")))
	(gvm:gvm-export-g-file repo filename tmpnam gvm:+latest-version+)
	(with-open-file (f tmpnam :element-type '(unsigned-byte 8) :if-does-not-exist :error)
	  (let ((arr (make-array (+ (file-length f) 1) :element-type '(unsigned-byte 8))))
	    (gsnet:writemsg s (make-instance 'gsnet:gmmsg :manifest (list filename)))     
	    (read-sequence arr f)
	    (gsnet:writemsg s (make-instance 'gsnet:gcmsg :chunk arr :reuuid reuuid))))
	(sb-posix:unlink tmpnam)
	(gvm:gvm-close-repo repo))
    (gvm::no-such-repo (p) (gsnet:writemsg s (make-instance 'gsnet:failmsg)))))

(defun send-bot-geom (s reuuid filename)
  (gsnet:writemsg s (make-instance 'gsnet:failmsg)))

(defun send-ls (s uri)
  ;;; need some more stuff from gvm
  (gsnet:writemsg s (make-instance 'gsnet:lsrmsg :manifest '("Some stuff" "some other stuff"))))

(defun handle-packet (s m)
  (when *verbose* (format t "Handling ~a~%" (type-of m)))
  (cond
    ((equalp (type-of m) 'gsnet:grmsg) (send-geom s (gsnet::uuid m) (gsnet::uri m)))
    ((equalp (type-of m) 'gsnet:gbrmsg) (send-bot-geom s (gsnet::uuid m) (gsnet::uri m)))
    ((equalp (type-of m) 'gsnet:lsmsg) (send-ls s (gsnet::uri m)))
    ((equalp m t) m)
    ((equalp m '()) m)
    (t (format t "Unhandled thing ~a~%" (type-of m)))))

(defun handle-connection (st)
  (let ((s (make-instance 'gsnet:session :stream st)))
    
    ;;; initial handshane and authentication
    (setf (gsnet::sessionuuid s) (format '() "~a" (uuid:make-v4-uuid)))
    (setf (gsnet::localnode s) *nodename*)
    (gsnet:writemsg s (make-instance 'gsnet:rnnsetmsg :name *nodename*))
    (unless (gsnet:readmsg s) (return-from handle-connection '()))
    (let ((m (gsnet:readmsg s)))
      (if (equalp (type-of m) 'gsnet:nsrmsg)
	  (unless (authenticate s (gsnet::username m) (gsnet::password m)) (return-from handle-connection '()))
	  (return-from handle-connection '())))
    (gsnet:writemsg s (make-instance 'gsnet::infomsg :sessionuuid (gsnet::sessionuuid s)))
    
    ;;; main loop
    (loop while (handle-packet s (gsnet:readmsg s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run (&key (listenhost #(127 0 0 1)) (port 5309) (multi-threading t) (in-new-thread t))
  (usocket:socket-server listenhost port #'handle-connection  '() :element-type 'unsigned-byte :multi-threading multi-threading :in-new-thread in-new-thread :reuse-address t))

(defun stop ()
  (map 'nil (lambda (th) 
	 (cond 
	   ((equalp (sb-thread:thread-name th) "USOCKET Client") (sb-thread:terminate-thread th))
	   ((equalp (sb-thread:thread-name th) "USOCKET Server") (sb-thread:terminate-thread th))))
       (sb-thread:list-all-threads)))
