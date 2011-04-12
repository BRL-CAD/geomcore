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

(cffi:define-foreign-library libgcv
   (:darwin (:or "libgcv.19.dylib" "libgcv.dylib"))
   (:unix (:or "libgcv.19.so" "libgcv.so"))
   (:windows "libgcv.dll")
   (t (:default "libgcv")))
(cffi:use-foreign-library libgcv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +rt-ap-magic+ #x4170706c) ; appl

(defcstruct xray "Ray"
  (magic :unsigned-long)
  (index :int)
  (r_pt :double :count 3)
  (r_dir :double :count 3)
  (r_min :double)
  (r_man :double))

(defcstruct application "Application structure"
  (a_magic :unsigned-long)
  
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
(defcfun "rt_prep" :void (rti :pointer))
(defcfun "rt_gettree" :int (rti :pointer) (reg :string))
(defcfun "rt_clean" :void (rti :pointer))
(defcfun "rt_free_rti" :void (rti :pointer))
(defcfun "rt_shootray" :int (ap :pointer))

(defcfun "nmg_booltree_leaf_tess" :pointer (tsp :pointer) (pathp :pointer) (ip :pointer) (client-data :pointer))
(defcallback nmg-booltree-leaf-tess-trampoline :pointer ((tsp :pointer) (pathp :pointer) (ip :pointer) (client-data :pointer)) (nmg-booltree-leaf-tess tsp pathp ip client-data))

(defcfun "gcv_region_end" :pointer (tsp :pointer) (pathp :pointer) (curtree :pointer) (client_data :pointer))
(defcallback gcv-region-end-trampoline :pointer ((tsp :pointer) (pathp :pointer) (curtree :pointer) (client_data :pointer)) (gcv-region-end tsp pathp curtree client_data))
(defcfun "gcv_region_end_mc" :pointer (tsp :pointer) (pathp :pointer) (curtree :pointer) (client_data :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rt-open (filename regions &key hitfunc missfunc multioverlapfunc logoverlapfunc prep)
  (with-foreign-object (a 'application)
					; RT_APPLICATION_INIT
    (loop for i from 0 to (- (foreign-type-size 'application) 1) do (setf (mem-aref a :unsigned-char i) 0))
    (setf (foreign-slot-value a 'application 'a_magic) +rt-ap-magic+)
    
					; set handler funcs
    (when hitfunc (setf (foreign-slot-value a 'application 'a_hit) (cffi:get-callback hitfunc)))
    (when missfunc (setf (foreign-slot-value a 'application 'a_miss) (cffi:get-callback missfunc)))
    (when multioverlapfunc (setf (foreign-slot-value a 'application 'a_multioverlap) (cffi:get-callback multioverlapfunc)))
    (when logoverlapfunc (setf (foreign-slot-value a 'application 'a_logoverlap) (cffi:get-callback logoverlapfunc)))
    
					; load/prep
    (setf (foreign-slot-value a 'application 'a_rt_i) (rt-dirbuild filename "RT" 0))
    (loop for region in regions do (rt-gettree (foreign-slot-value a 'application 'a_rt_i) region))
    (when prep (rt-prep-parallel (foreign-slot-value a 'application 'a_rt_i) 8))
    a))

(defun facetize (dbip names)
  (let ((ts '()))
    (db-walk-tree dbip 1 names 1 ts 0 
		  (get-callback 'gcv-region-end-trampoline)
		  (get-callback 'nmg-booltree-leaf-tess-trampoline)
		  (get-callback 'gcv-writer))))
