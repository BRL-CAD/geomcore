;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem gsserver
 :name "gsserver"
 :version "0.0.0"
 :maintainer "Erik G"
 :author "Erik G"
 :licence "BSD sans advertising clause (see file COPYING for details)"
 :description "GeometryService server"
 :long-description "Common Lisp server for the BRL-CAD Geometry Service protocol"
 :serial t
 :depends-on (:cffi :usocket :uuid :gvm :sb-posix)
 :components ((:file "gsnet")
              (:file "gsserver")))

