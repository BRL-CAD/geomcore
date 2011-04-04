;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem brlcad
 :name "brlcad"
 :version "0.0.0"
 :maintainer "Erik G"
 :author "Erik G"
 :licence "BSD sans advertising clause (see file COPYING for details)"
 :description "GeometryService client"
 :long-description "Common Lisp client interface for the BRL-CAD Geometry Service protocol"
 :serial t
 :depends-on (:cffi)
 :components ((:file "brlcad")))

