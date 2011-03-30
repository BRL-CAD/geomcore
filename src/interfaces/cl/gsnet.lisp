;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsnet
  (:use :cl :sb-unix)
  (:export :connect :writemsg :readmsg
	   :session :message :pingmsg :pongmsg :nodenamemsg :loginmsg :logoutmsg :rualivemsg :imalivemsg :okmsg :failmsg :geomreqmsg :geommanifestmsg :geomchunkmsg
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
(defconstant +gsgm+      #x0405) ; Geometry Manifest
(defconstant +gsgc+      #x0410) ; Geometry Chunk

(defun usec () (multiple-value-bind (_ sec usec) (sb-unix:unix-gettimeofday) (declare (ignore _)) (+ (* 1000000 sec) usec)))

;;; utility functions to write out
(defun writeuint64 (s i) (loop for a in '(56 48 40 32 24 16 8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writeuint32 (s i) (loop for a in '(24 16 8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writeuint16 (s i) (loop for a in '(8 0) do (write-byte (ldb (byte 8 a) i) s)))
(defun writegsstring (s str)  (writeuint32 s (length str)) (loop for x being the element of str do (if x (write-byte (char-code x) s))))

;;; utility functions to read in
(defun readuint64 (s) (loop with i = 0 for a in '(56 48 40 32 24 16 8 0) do (dpb (read-byte s) (byte 8 a) i) finally (return i)))
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
  ((localnode :accessor localnode :initform +nodename+)
   (remotenode :accessor remotenode)
   (sessionuuid :accessor sessionuuid :initform '())
   (username  :accessor username :initarg :username)
   (password :accessor password :initarg :password)
   (host :accessor host :initarg :host)
   (port :accessor port :initarg :port)
   (strm :accessor strm :initform 'nil)
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
	    (reuuid (if (= (read-byte (strm s)) 1) (readuuid (strm s)) '())))
	(setf length (- length (+ 2 4 (length uuid) (if reuuid (+ (length reuuid) 4) 0) 1)))
	(cond 
	  ((= type +gsrnnset+) (setf (remotenode s) (readgsstring (strm s))) type)
	  ((= type +gspong+) (make-instance 'pongmsg :tv (readuint64 (strm s))))
	  ((= type +gsping+) (writemsg s (make-instance 'pongmsg :tv (readuint64 (strm s))))) ; automatically respond to ping requests
	  ((= type +gsinfo+) (setf (sessionuuid s) (readgsstring (strm s))) type)
	  ((= type +gsfail+) (make-instance 'failmsg))
	  ((= type +gsok+) (make-instance 'okmsg))
	  ((= type +gsrualive+) (writemsg s (make-instance 'imalivemsg))) ; automatically respond to rualive 
	  ((= type +gsimalive+) (make-instance 'imalivemsg))
	  ((= type +gsgr+) (make-instance 'geomreqmsg :uri (readgsstring (strm s))))
	  ((= type +gsgm+) (make-instance 'geommanifestreq :manifest (loop for i from 0 to (readuint32 (strm s)) collect (readgsstring (strm s)))))
	  ((= type +gsgc+) (make-instance 'geomchunkmsg :chunk (let ((len (readuint32 (strm s)))) (loop with c = (make-array len :element-type '(unsigned-byte 8)) for i from 0 to len do (setf (aref c i) (read-byte (strm s)))))))
	  (t (format t "Unknown type! ~x~%" type))))
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
(defclass pingmsg (message) ((tv :accessor tv :initform (usec))))
(defmethod writemsg :before (s (m pingmsg)) (setf (msgtype m) +gsping+) (setf (len m) 8))
(defmethod writemsg :after (s (m pingmsg)) (format t "Pinging with ~a~%" (tv m)) (writeuint64 (strm s) (tv m)))

(defclass pongmsg (message) ((tv :accessor tv :initarg :tv)))
(defmethod writemsg :before (s (m pongmsg)) (setf (msgtype m) +gspong+) (setf (len m) 8))
(defmethod writemsg :after (s (m pongmsg)) (writeuint64 (strm s) (tv m)))

(defclass nodenamemsg (message) ((name :accessor name :initarg :name)))
(defmethod writemsg :before (s (m nodenamemsg)) (setf (msgtype m) +gsrnnset+) (setf (len m) (+ (length (localnode s)) 4)))
(defmethod writemsg :after (s (m nodenamemsg)) (writegsstring (strm s) (localnode s)))

(defclass loginmsg (message) ())
(defmethod writemsg :before (s (m loginmsg)) (setf (msgtype m) +gsnsr+) (setf (len m) (+ (length (username s)) (length (password s)) 8)))
(defmethod writemsg :after (s (m loginmsg)) (writegsstring (strm s) (username s)) (writegsstring (strm s) (password s)))

(defclass logoutmsg (message) ())
(defmethod writemsg :before (s (m logoutmsg)) (setf (msgtype m) +gsdr+))

(defclass rualivemsg (message) ())
(defmethod writemsg :before (s (m logoutmsg)) (setf (msgtype m) +gsrualive+))

(defclass imalivemsg (message) ())
(defmethod writemsg :before (s (m logoutmsg)) (setf (msgtype m) +gsimalive+))

(defclass okmsg (message) ())
(defmethod writemsg :before (s (m logoutmsg)) (setf (msgtype m) +gsok+))

(defclass failmsg (message) ())
(defmethod writemsg :before (s (m logoutmsg)) (setf (msgtype m) +gsfail+))

(defclass geomreqmsg (message) ((uri :accessor uri :initarg :uri :initform "")))
(defmethod writemsg :before (s (m geomreqmsg)) (setf (msgtype m) +gsgr+) (setf (len m) (+ (length (uri m)) 4)))
(defmethod writemsg :after (s (m geomreqmsg)) (writegsstring (strm s) (uri m)))

(defclass geommanifestmsg (message) ((manifest :accessor manifest :initarg :manifest)))
(defmethod writemsg :before (s (m geommanifestmsg)) 
  (setf (msgtype m) +gsgm+)
  (setf (len m) (apply #'+ 4 (mapcar (lambda (x) (+ (length x) 4)) (manifest m)))))
(defmethod writemsg :after (s (m geomreqmsg)) (writeuint32 (strm s) (length (manifest m))) 
	   (loop for i in (manifest m) do (writegsstring (strm s) i)))

(defclass geomchunkmsg (message) ((chunk :accessor chunk :initarg :chunk)))
(defmethod writemsg :before (s (m geomchunkmsg)) (setf (msgtype m) +gsgc+) (setf (len m) (length (chunk m))))
(defmethod writemsg :after (s (m geomchunkmsg)) (writeuint32 (strm s) (length (chunk m))) (loop for i from 0 to (length (chunk m)) do (write-byte (aref (chunk m) i) (strm s))))

