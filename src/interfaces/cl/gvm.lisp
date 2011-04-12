;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gvm
 (:use :cl :cffi)
 (:export 
	:gvm_info_init
	:gvm_info_free
	:gvm_info_clear_objects
	:gvm_init_repo
	:gvm_open_repo
	:gvm_object_in_model
	:gvm_get_extern_obj
	:gvm_get_repo_obj
	:gvm_diff
	:gvm_add_to_list
	:gvm_commit_objs
	:gvm_new_model
	:gvm_get_model
	:gvm_get_objs
	:gvm_import_g_file
	:gvm_commit_g_file
	:gvm_export_g_file
	:gvm_export_list
	:gvm_export_object))

(in-package :gvm)

(pushnew #P"/usr/brlcad/lib/" cffi:*foreign-library-directories* :test #'equal)
#+darwin(pushnew #P"/usr/brlcad/lib/" cffi:*darwin-framework-directories* :test #'equal)

(cffi:define-foreign-library libgvm 
   (:darwin (:or "libgvm.19.dylib" "libgvm.dylib"))
   (:unix (:or "libgvm.19.so" "libgvm.so"))
   (:windows "libgvm.dll")
   (t (:default "libgvm")))
(cffi:use-foreign-library libgvm)

(defctype size_t :unsigned-int)

(defcfun "gvm_info_init" :void (repo_info :pointer))
(defcfun "gvm_info_free" :void (repo_info :pointer))
(defcfun "gvm_info_clear_objects" :void (repo_info :pointer))
(defcfun "gvm_init_repo" :int (repo_info :pointer) (repo_path :string))
(defcfun "gvm_open_repo" :int (repo_info :pointer) (repo_path :string))
(defcfun "gvm_object_in_model" :int (repo_info :pointer) (model_name :string) (obj_name :string) (ver_num size_t))
(defcfun "gvm_get_extern_obj" :pointer (repo_info :pointer) (model_name :string) (obj_name :string) (ver_num size_t))
(defcfun "gvm_get_repo_obj" :pointer (repo_info :pointer) (model_name :string) (obj_name :string) (ver_num size_t))
(defcfun "gvm_diff" :int (repo_info :pointer) (obj1 :pointer) (obj2 :pointer))
(defcfun "gvm_add_to_list" :int (repo_info :pointer) (obj :pointer))
(defcfun "gvm_commit_objs" :int (repo_info :pointer))
(defcfun "gvm_new_model" :int (repo_info :pointer) (model_name :string))
(defcfun "gvm_get_model" :int (repo_info :pointer) (model_name :string) (ver_num size_t))
(defcfun "gvm_get_objs" :int (repo_info :pointer) (model_name :string) (obj_name :string) (ver_num size_t) (recursive :int))
(defcfun "gvm_import_g_file" :int (repo_info :pointer) (g_file :string))
(defcfun "gvm_commit_g_file" :int (repo_info :pointer) (model_name :string) (g_file :string))
(defcfun "gvm_export_g_file" :int (repo_info :pointer) (model_name :string) (g_file :string) (ver_num size_t))
(defcfun "gvm_export_list" :int (repo_info :pointer) (model_name :string) (g_file :string))
(defcfun "gvm_export_object" :int (repo_info :pointer) (model_name :string) (obj_name :string) (g_file :string) (ver_num size_t) (recursive :int))
