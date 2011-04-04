;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :brlcad
 (:use :cl :cffi)
 (:export :db-open))

(in-package :brlcad)

(pushnew #P"/usr/brlcad/lib/" cffi:*foreign-library-directories* :test #'equal)
#+darwin(pushnew #P"/usr/brlcad/lib/" cffi:*darwin-framework-directories* :test #'equal)

(cffi:define-foreign-library librt 
   (:darwin (:or "librt.19.dylib" "librt.dylib"))
   (:unix (:or "librt.19.so" "librt.so"))
   (:windows "librt.dll")
   (t (:default "librt")))
(cffi:use-foreign-library librt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct xray "Ray"
  (magic :uint32)
  (index :int)
  (r_pt :double :count 3)
  (r_dir :double :count 3)
  (r_min :double)
  (r_man :double))

(defcstruct application "Application structure"
  (a_magic :uint32)
  
  ;;; THESE ELEMENTS ARE MANDATORY
  (a_ray xray)		; Actual ray to be shot
  
  (a_hit :pointer)
  (a_miss :pointer)
  
  (a_onehit :int)		; flag to stop on first hit
  (a_ray_length :double) ; distance from ray start to end :intersections
  (a_rt_i :pointer)		; this librt instance
  (a_zero1 :int)		; must be zero (sanity check)
  ;;; THESE ELEMENTS ARE USED BY THE LIBRARY, BUT MAY BE LEFT ZERO
  (a_resource :pointer)	; dynamic memory resources
  
  (a_overlap :pointer)
  (a_multioverlap :pointer)
  (a_logoverlap :pointer)
  
  (a_level :int)	     ; recursion level (for pr:inting)
  (a_x :int)		      ; Screen X of ray, if applicable
  (a_y :int)		      ; Screen Y of ray, if applicable
  (a_purpose :string)		; Debug string: purpose of ray
  (a_rbeam :double)		; initial beam radius (mm)
  (a_diverge :double)		; slope of beam divergance/mm
  (a_return :int)		; Return of a_hit()/a_miss()
  (a_no_booleans :int)    ; 1= partitions==segs, no booleans
  (attrs :pointer) ; null terminated list of attributes This list should be the same as passed to rt_gettrees_and_attrs()
  
  ;;; THESE ELEMENTS ARE USED BY THE PROGRAM "rt" AND MAY BE USED BY
  ;;; THE LIBRARY AT SOME FUTURE DATE
  ;;; AT THIS TIME THEY MAY BE LEFT ZERO
  (a_pixelext :pointer)	; locations of pixel corners
  
  ;;; THESE ELEMENTS ARE WRITTEN BY THE LIBRARY, AND MAY BE READ IN a_hit()
  (a_finished_segs_hdp :pointer)
  (a_Final_Part_hdp :pointer)
  (a_inv_dir :double :count 3) ; filled in by rt_shootray(), inverse of ray direction cosines
  
  ;;; THE FOLLOWING ELEMENTS ARE MAINLINE & APPLICATION SPECIFIC.
  ;;; THEY ARE NEVER EXAMINED BY THE LIBRARY.
  (a_user :int)		; application-specific value
  (a_uptr :pointer)	       ; application-specific pointer
  (a_spectrum :pointer)
  (a_color :double :count 3)	; application-specific color
  (a_dist :double)	       ; application-specific distance
  (a_uvec :double :count 3)	; application-specific vector
  (a_vvec :double :count 3)	; application-specific vector
  (a_refrac_index :double)	; current index of refraction
  (a_cumlen :double)		; cumulative length of ray
  (a_flag :int)		; application-specific flag
  (a_zero2 :int))		; must be zero (sanity check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "db_open" :pointer (file :string) (mode :string))
(defcfun "db_close" :void (dbip :pointer))
(defcfun "db_dirbuild" :int (dbip :pointer))
(defcfun "db_walk_tree" :void
  (dbip :pointer)
  (argc :int)
  (argv :string)
  (ncpu :int)
  (tree_state :pointer)
  (start-func :pointer)
  (end-func :pointer)
  (leaf-func :pointer)
  (client-data :pointer))
(defcfun "db_version" :int (dbip :pointer))

(defcfun "rt_dirbuild" :pointer (file :string) (descr :string) (i :int))
(defcfun "rt_prep_parallel" :void (rti :pointer) (ncpu :int))
(defcfun "rt_gettree" :int (rti :pointer) (reg :string))
(defcfun "rt_free_rti" :void (rti :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rt-open (filename regions)
  (let ((a (foreign-alloc (foreign-type-size 'application))))
    (setf (application-a_rt_i a) (rt-dirbuild filename "RT" 0))
    (loop for region in regions do (rt-gettree (application-a_rt_i a) region))
    a))
