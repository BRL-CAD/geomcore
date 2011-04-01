/*                      G V M . H
 * BRL-CAD
 *
 * Copyright (c) 1993-2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file gvm.h
 *
 * Geometry Version Management library.
 *  
 */

#ifndef GVM_EXPORT
#  if defined(_WIN32) && !defined(__CYGWIN__) && defined(BRLCAD_DLL)
#    ifdef GVM_EXPORT_DLL
#      define GVM_EXPORT __declspec(dllexport)
#    else
#      define GVM_EXPORT __declspec(dllimport)
#    endif
#  else
#    define GVM_EXPORT
#  endif
#endif

#define GVM_EXTERN(type_and_name, args) extern type_and_name args


/* Convenient definitions */
#define LATEST_VERSION 0


/**
 * Repository object(s)
 *
 * A bu_list enabled structure to hold repository items in memory.
 *
 */
struct repository_objects {
   struct bu_list l;
   char *model_name;
   char *obj_name;
   size_t version;
   struct bu_external *contents;
   int action; /* 0 = update, 1 = add, 2 = delete  - used for commit operations*/
}


/**
 * Repository Structure
 *
 * This structure holds all state information pertaining to a
 * geometry repository
 */

struct gvm_info {
   void *internal;			/* Internal information used by the backend */
   char *repo_full_path;		/* Full filesystem path to repository */
   struct repository_objects *objects;	/* List of currently "active" items */
}

/**
 * Utility routines for repository structures
 */

/* Initialize the info structure - backend will
 * initialize whatever internals it needs as well */
GVM_EXPORT GVM_EXTERN(void gvm_info_init,
		(struct gvm_info *repo_info));
  
/* Free all items in the gvm_info structure */ 
GVM_EXPORT GVM_EXTERN(void gvm_info_free,
		(struct gvm_info *repo_info));

/* Clear repository items list */
GVM_EXPORT GVM_EXTERN(void gvm_info_clear_items,
		(struct gvm_info *repo_info));


/**
 * GVM Repository Functions
 */

/* Create a repository at the filesystem location repo_path */
GVM_EXPORT GVM_EXTERN(int gvm_init_repo,
		(struct gvm_info *repo_info, 
		 const char *repo_path));

/* Open a repository. Note that this may change to add some
 * sort of authentication paramaters. */
GVM_EXPORT GVM_EXTERN(int gvm_open_repo,
		(struct gvm_info *repo_info, 
		 const char *repo_path));

/**
 * GVM .g file helper routines
 */ 

/* Populate a new model repository using a .g file. */
GVM_EXPORT GVM_EXTERN(int gvm_import_g_file,
		(struct gvm_info *repo_info, 
		 const char *g_file));

/* List objects present in a .g file but not in the corresponding
 * model repository. */
GVM_EXPORT GVM_EXTERN(int gvm_g_file_get_add_list,
		(struct gvm_info *repo_info, 
		 const char *g_file));

/* List objects present in a model repository but not in the 
 * corresponding .g file. */
GVM_EXPORT GVM_EXTERN(int gvm_g_file_get_delete_list,
		(struct gvm_info *repo_info, 
		 const char *g_file));

/* List objects present in both a model repository and in the 
 * corresponding .g file that differ. */
GVM_EXPORT GVM_EXTERN(int gvm_g_file_get_diff_list,
		(struct gvm_info *repo_info, 
		 const char *g_file));

/* Update an existing model repository using a .g file. */
GVM_EXPORT GVM_EXTERN(int gvm_commit_g_file,
		(struct gvm_info *repo_info, 
		 const char *g_file));

/* Export a complete model repository to a .g file. If ver_num
 * is LATEST_VERSION use latest revision */
GVM_EXPORT GVM_EXTERN(int gvm_export_g_file,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 const char *g_file,
		 size_t ver_num));

/* Export a subset of a model repository to a .g file. If ver_num
 * is LATEST_VERSION use latest revision */
GVM_EXPORT GVM_EXTERN(int gvm_export_object,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 const char *obj_name, 
		 const char *g_file,
		 size_t ver_num,
		 int recursive));

/**
 * GVM object level routines
 */

/* Populate a repository_objects struct with the contents
 * of obj_name.  This is how to go from svn's internal
 * stored copy of an object to the bu_external needed for
 * BRL-CAD .g level operations.*/
GVM_EXPORT GVM_EXTERN(struct repository_objects * gvm_get_repo_obj,
		(struct gvm_info *repo_info,
		 const char *model_name,
		 const char *obj_name,
		 size_t ver_num));

/* Check if an object exists in a given model as defined
 * in revision ver_num.  If ver_num is LATEST_VERSION use
 * latest revision */
GVM_EXPORT GVM_EXTERN(int gvm_object_in_model,
	       (struct gvm_info *repo_info,
		const char *model_name,
		const char *obj_name,
		size_t ver_num));

/* Very simple routine to determine if two objects
 * are different - based on binary data, not geometry
 * aware.  Returns 0 if they're the same and one if
 * they differ */
GVM_EXPORT GVM_EXTERN(int gvm_diff,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj1,
		struct repository_objects *obj2));


/* Add a repository_objects struct to repo_info's
 * objects list */
GVM_EXPORT GVM_EXTERN(int gvm_add_to_list,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj));


	
