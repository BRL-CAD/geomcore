;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gsclient
  (:use :cl :sb-unix)
  (:export :login))

(in-package :gsclient)

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

;;; type specific send handling
(defclass pingmsg (message) ((tv :accessor tv :initform (usec))))
(defmethod writemsg :before (s (m pingmsg)) (setf (msgtype m) +gsping+) (setf (len m) 8))
(defmethod writemsg :after (s (m pingmsg)) (writeuint64 s (tv m)))

(defclass pongmsg (message) ((tv :accessor tv :initarg tv)))

(defclass nodenamemsg (message) ((name :accessor name :initarg :name)))
(defmethod writemsg :before (s (m nodenamemsg)) (setf (msgtype m) +gsrnnset+) (setf (len m) (+ (length (localnode s)) 4)))
(defmethod writemsg :after (s (m nodenamemsg)) (writegsstring (strm s) (localnode s)))

(defclass loginmsg (message) ())
(defmethod writemsg :before (s (m loginmsg)) (setf (msgtype m) +gsnsr+) (setf (len m) (+ (length (username s)) (length (password s)) 8)))
(defmethod writemsg :after (s (m loginmsg)) (writegsstring (strm s) (username s)) (writegsstring (strm s) (password s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  public interface  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun login (&key (username "Guest") (password "Guest") (host #(127 0 0 1)) (port 5309))
  (let ((s (make-instance 'session :host host :port port :username username :password password)))
    (setf (socket s) (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
    (setf (strm s) (usocket:socket-stream (socket s)))
    (writemsg s (make-instance 'nodenamemsg :name (localnode s)))
    (readmsg s)
    (format t "Remote name: ~a~%" (remotenode s))
    (writemsg s (make-instance 'loginmsg))))

(defun logout (s)
  (usocket:socket-close s))
