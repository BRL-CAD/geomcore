;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem gvm
 :name "gvm"
 :version "0.0.0"
 :maintainer "Erik G"
 :author "Erik G"
 :licence "BSD sans advertising clause (see file COPYING for details)"
 :description "GeometryService version manager"
 :long-description "Common Lisp client interface for the BRL-CAD Geometry Service version management stuff"
 :serial t
 :depends-on (:cffi)
 :components ((:file "gvm")))

