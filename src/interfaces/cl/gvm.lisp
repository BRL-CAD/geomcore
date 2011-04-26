;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(in-package :cl-user)

(defpackage :gvm
 (:use :cl :cffi)
 (:export 
	:gvm-info-init
	:gvm-info-free
	:gvm-info-clear-objects
	:gvm-init-repo
	:gvm-open-repo
	:gvm-close-repo
	:gvm-object-in-model
	:gvm-get-extern-obj
	:gvm-get-repo-obj
	:gvm-diff
	:gvm-add-to-list
	:gvm-commit-objs
	:gvm-new-model
	:gvm-get-model
	:gvm-get-objs
	:gvm-import-g-file
	:gvm-commit-g-file
	:gvm-export-g-file
	:gvm-export-list
	:gvm-export-object

	:+latest-version+

	:gvm-open))

(in-package :gvm)

(pushnew #P"/usr/brlcad/lib/" cffi:*foreign-library-directories* :test #'equal)
#+darwin(pushnew #P"/usr/brlcad/lib/" cffi:*darwin-framework-directories* :test #'equal)

(cffi:define-foreign-library libgvm 
   (:darwin "libgvm.dylib")
   (:unix "libgvm.so")
   (:windows "libgvm.dll")
   (t (:default "libgvm")))
(cffi:use-foreign-library libgvm)

(defctype size_t :unsigned-int)

(defconstant +latest-version+ 0)
(defconstant +gvm-update-obj+ 1)
(defconstant +gvm-add-obj+ 2)
(defconstant +gvm-del-obj+ 3)

(defcfun "gvm_info_init" :void (repo_info :pointer))
(defcfun "gvm_info_free" :void (repo_info :pointer))
(defcfun "gvm_info_clear_objects" :void (repo_info :pointer))
(defcfun "gvm_init_repo" :int (repo_info :pointer) (repo_path :string))
(defcfun "gvm_open_repo" :int (repo_info :pointer) (repo_path :string))
(defcfun "gvm_close_repo" :int (repo_info :pointer))
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

; attempt to open a repo, creating if necessary
(define-condition no-such-repo (error) ((path :initarg :path :reader path)))
(defun gvm-open (path)
  (let ((info (foreign-alloc :pointer :count 3)))
    (if (= (gvm-open-repo info path) 0)
	info
	(if (= (gvm-init-repo info path) 0)
	    info
	    (error 'no-such-repo :path path)))))

(defparameter +repo-file+ "./GS_repository")
(defun test (file &key (something '()))
  (time
   (let ((info (gvm-open +repo-file+)))
     (gvm-new-model info "test.g")
     (time (gvm-import-g-file info file))
     (when something (gvm-commit-g-file info file something))
     (time
      (progn
	(gvm-export-g-file info file "test.g" +latest-version+)
	(gvm-export-object info file "hull" "tank_obj.g" +latest-version+ 0)
	(gvm-export-object info file "tank" "tank.g" +latest-version+ 1)
	))
     (gvm-close-repo info)
     (foreign-free info))))

