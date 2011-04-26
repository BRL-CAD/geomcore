;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsnet
  (:use :cl :sb-unix)
  (:export :connect :writemsg :readmsg
	   :session :message
	   :pingmsg :pongmsg :rnnsetmsg :nsrmsg :drmsg :rualivemsg :imalivemsg :okmsg :failmsg :grmsg :gbrmsg :gmmsg :gcmsg :lsmsg :lsrmsg
	   :manifest :remotenode :sessionuuid :socket :strm
	   :usec))

(in-package :gsnet)

(defparameter +nodename+ "Geist")
(defconstant +magic+ #x41fe5309)

;;; message types
(defconstant +gsrualive+ #x0042) ; Test if server is up
(defconstant +gsimalive+ #x0043) ; Expected response from running server to GSRUALIVE
(defconstant +gsfail+    #x0050) ; Failure
(defconstant +gsok+      #x0051) ; Success
(defconstant +gsping+    #x0060) ; Ping
(defconstant +gspong+    #x0062) ; Pong
(defconstant +gsrnnset+  #x0100) ; GS Remote Nodename Set
(defconstant +gsdr+      #x0150) ; Disconnect Request
(defconstant +gsnnnet+   #x0200) ; New Node on Network
(defconstant +gsfnlr+    #x0250) ; Full Nodename List Request (Not implemented yet)
(defconstant +gsfnl+     #x0255) ; Full Nodename List (Not implemented yet)
(defconstant +gsnsr+     #x0300) ; New Session Request
(defconstant +gsinfo+    #x0305) ; Session Information
(defconstant +gsgr+      #x0400) ; Geometry Request
(defconstant +gsgbr+     #x0401) ; Geometry BoT Request
(defconstant +gsls+      #x0402) ; Geometry list request
(defconstant +gslsr+     #x0403) ; Geometry list response
(defconstant +gsgm+      #x0405) ; Geometry Manifest
(defconstant +gsgc+      #x0410) ; Geometry Chunk

(defun usec () (multiple-value-bind (_ sec usec) (sb-unix:unix-gettimeofday) (declare (ignore _)) (+ (* 1000000 sec) usec)))

;;; utility functions to write out
(defun writeuint64 (s i) (loop for a in '(56 48 40 32 24 16 8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writeuint32 (s i) (loop for a in '(24 16 8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writeuint16 (s i) (loop for a in '(8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writegsstring (s str)  (writeuint32 s (length str)) (loop for x being the element of str do (when x (write-byte (char-code x) s))))

;;; utility functions to read in
(defun readuint64 (s) (apply #'+ (loop for a in '(56 48 40 32 24 16 8 0) collect (dpb (read-byte s) (byte 8 a) 0))))
(defun readuint32 (s) (+ (* (read-byte s) #x1000000) (* (read-byte s) #x10000) (* (read-byte s) #x100) (read-byte s)))
(defun readuint16 (s) (+ (* #x100 (read-byte s)) (read-byte s)))
(defun readgsstring (s) 
  (let ((length (readuint32 s)))
    (loop with str = (make-string length) for i from 0 to (- length 1)
       do (setf (char str i) (code-char (read-byte s))) finally (return str))))
(defun readuuid (s) (uuid:make-uuid-from-string (readgsstring s)))
(defun readmagic (s) (= +magic+ (readuint32 s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass session ()
  ((localnode :accessor localnode :initform +nodename+ :initarg :localnode)
   (remotenode :accessor remotenode)
   (sessionuuid :accessor sessionuuid :initform '())
   (username  :accessor username :initarg :username)
   (password :accessor password :initarg :password)
   (host :accessor host :initarg :host)
   (port :accessor port :initarg :port)
   (strm :accessor strm :initform 'nil :initarg :stream)
   (socket :accessor socket :initform 'nil)))

(defclass message ()
  ((msgtype :accessor msgtype :initarg :msgtype)
   (uuid    :accessor uuid    :initarg :uuid :initform (format '() "~a" (uuid:make-v4-uuid)))
   (reuuid  :accessor reuuid  :initarg :reuuid :initform '())
   (len     :accessor len     :initform 0)))

;;; snarf data off the line and return an instance of the right kind of class
(defun readmsg (s)
  (if (readmagic (strm s))
      (let ((length (readuint32 (strm s)))
	    (type (readuint16 (strm s)))
	    (uuid (readgsstring (strm s)))
	    (reuuid (when (= (read-byte (strm s)) 1) (readuuid (strm s)) '())))
	(declare (ignore length))
	(declare (ignore uuid))
	(declare (ignore reuuid))
	(cond
	  ((= type +gsrnnset+)	(setf (remotenode s) (readgsstring (strm s))) t)
	  ((= type +gsdr+)	(writemsg s (make-instance 'logoutmsg)) '())
	  ((= type +gspong+)	(make-instance 'pongmsg :tv (readuint64 (strm s))))
	  ((= type +gsping+)	(writemsg s (make-instance 'pongmsg :tv (readuint64 (strm s)))) t) ; automatically respond to ping requests
	  ((= type +gsinfo+)	(setf (sessionuuid s) (readgsstring (strm s))) t)
	  ((= type +gsfail+)	(make-instance 'failmsg))
	  ((= type +gsok+)	(make-instance 'okmsg))
	  ((= type +gsrualive+)	(writemsg s (make-instance 'imalivemsg)) t) ; automatically respond to rualive 
	  ((= type +gsimalive+)	(make-instance 'imalivemsg))
	  ((= type +gsgr+)	(make-instance 'grmsg :uri (readgsstring (strm s))))
	  ((= type +gsgbr+)	(make-instance 'gbrmsg :uri (readgsstring (strm s))))
	  ((= type +gsgm+)	(make-instance 'gmmsg :manifest (loop for i from 1 to (readuint32 (strm s)) collect (readgsstring (strm s)))))
	  ((= type +gsgc+)	(make-instance 'gcmsg :chunk 
					  (let ((arr (make-array (+	(readuint32 (strm s)) 1) :element-type '(unsigned-byte 8))))
						(read-sequence arr (strm s))
						arr)))
	  ((= type +gsls+)	(make-instance 'lsmsg :uri (readgsstring (strm s))))
	  ((= type +gslsr+)	(make-instance 'lsrmsg :manifest (loop for i from 1 to (readuint32 (strm s)) collect (readgsstring (strm s)))))
	  ((= type +gsnsr+)	(make-instance 'nsrmsg :username (readgsstring (strm s)) :password (readgsstring (strm s))))
	  (t (format t "Unknown type! ~a ~x~%" (type-of type) type))))
      '()))

(defgeneric writemsg (session message) (:documentation "Send the message to the socket stream"))

;;; common to all messages
(defmethod writemsg :before (s (m message)) (setf (len m) (+ (len m) 7 (length (uuid m)) (if (reuuid m) (+ (length (reuuid m)) 4) 0) )))
(defmethod writemsg (s (m message))
  (writeuint32 (strm s) +magic+) 
  (writeuint32 (strm s) (len m)) 
  (writeuint16 (strm s) (msgtype m)) 
  (writegsstring (strm s) (uuid m))
  (if (reuuid m)
      (progn (write-byte 1 (strm s)) (writegsstring (strm s) (reuuid m)))
      (write-byte 0 (strm s))))
(defmethod writemsg :around (s (m message)) (call-next-method) (force-output (strm s)))

;;; type specific send handling

(defmacro msg (name def size &optional send)
  (let ((classname (intern (format nil "~:@(~amsg~)" name)))
	(typename (intern (format nil "~:@(+gs~a+~)" name))))
    `(progn 
       (defclass ,classname (message) ,def)
       (defmethod writemsg :before (s (m ,classname)) (setf (msgtype m) ,typename) (setf (len m) ,size))
       (defmethod writemsg :after (s (m ,classname)) ,send))))

(msg ping ((tv :accessor tv :initform (usec))) 8 (writeuint64 (strm s) (tv m)))
(msg pong ((tv :accessor tv :initarg :tv)) 8 (writeuint64 (strm s) (tv m)))
(msg rnnset ((name :accessor name :initarg :name)) (+ (length (localnode s)) 4) (writegsstring (strm s) (localnode s)))
(msg nsr ((username :accessor username :initarg :username) (password :accessor password :initarg :password))
  (+ (length (username s)) (length (password s)) 8) (progn (writegsstring (strm s) (username s)) (writegsstring (strm s) (password s))))
(msg dr () 0)
(msg info ((sessionuuid :accessor sessionuuid :initarg :sessionuuid)) (+ (length (sessionuuid m)) 4) (writegsstring (strm s) (sessionuuid m)))
(msg rualive () 0)
(msg imalive () 0)
(msg ok () 0)
(msg fail () 0)
(msg gr ((uri :accessor uri :initarg :uri :initform "")) (+ (length (uri m)) 4) (writegsstring (strm s) (uri m)))
(msg gbr ((uri :accessor uri :initarg :uri :initform "")) (+ (length (uri m)) 4) (writegsstring (strm s) (uri m)))
(msg gm ((manifest :accessor manifest :initarg :manifest)) (apply #'+ 4 (mapcar (lambda (x) (+ (length x) 4)) (manifest m))) (progn (writeuint32 (strm s) (length (manifest m))) (loop for i in (manifest m) do (writegsstring (strm s) i))))
(msg gc ((chunk :accessor chunk :initarg :chunk)) (+ (length (chunk m)) 4) (progn (writeuint32 (strm s) (- (length (chunk m)) 1)) (write-sequence (chunk m) (strm s))))
(msg ls ((uri :accessor uri :initarg :uri :initform "")) (+ (length (uri m)) 4) (writegsstring (strm s) (uri m)))
(msg lsr ((manifest :accessor manifest :initarg :manifest)) (apply #'+ 4 (mapcar (lambda (x) (+ (length x) 4)) (manifest m))) (progn (writeuint32 (strm s) (length (manifest m))) (loop for i in (manifest m) do (writegsstring (strm s) i))))
