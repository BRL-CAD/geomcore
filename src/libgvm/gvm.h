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

#include "bu.h"

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
 * A bu_list enabled structure to hold repository objects in memory.
 *
 */
struct repository_objects {
   struct bu_list l;
   char *model_name;
   char *obj_name;
   size_t version;
   struct bu_external *contents;
   int action; /* 1 = update, 2 = add, 3 = delete  - used for commit operations*/
};


/**
 * Repository Structure
 *
 * This structure holds all state information pertaining to a
 * geometry repository
 */

struct gvm_info {
   void *internal;			/* Internal information used by the backend */
   const char *repo_full_path;		/* Full filesystem path to repository */
   struct repository_objects *objects;	/* List of currently "active" objects */
};

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

/* Clear repository objects list */
GVM_EXPORT GVM_EXTERN(void gvm_info_clear_objects,
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
 * GVM object level routines
 */

/* Check if an object exists in a given model as defined
 * in revision ver_num.  If ver_num is LATEST_VERSION use
 * latest revision */
GVM_EXPORT GVM_EXTERN(int gvm_object_in_model,
	       (struct gvm_info *repo_info,
		const char *model_name,
		const char *obj_name,
		size_t ver_num));

/* Return a bu_external structure with the contents
 * of obj_name.  This is how to go from a repository's internal
 * stored copy of an object to the bu_external needed for
 * BRL-CAD .g level operations.*/
GVM_EXPORT GVM_EXTERN(struct bu_external * gvm_get_extern_obj,
		(struct gvm_info *repo_info,
		 const char *model_name,
		 const char *obj_name,
		 size_t ver_num));


/* Return a repository object structure for a particular
 * object.  This function will set the model_name, object_name
 * and version of a repository object. Does no validation against
 * the repository */
GVM_EXPORT GVM_EXTERN(struct repository_objects * gvm_get_repo_obj,
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
		struct bu_external *obj1,
		struct bu_external *obj2));

/* Add a repository_objects struct to repo_info's
 * objects list */
GVM_EXPORT GVM_EXTERN(int gvm_add_to_list,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj));

/* Queue "Add object" action for commit */
GVM_EXPORT GVM_EXTERN(int gvm_add_obj,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj));

/* Queue "Delete object" action for commit */
GVM_EXPORT GVM_EXTERN(int gvm_delete_obj,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj));

/* Queue "Update object" action for commit */
GVM_EXPORT GVM_EXTERN(int gvm_update_obj,
	       (struct gvm_info *repo_info,
		struct repository_objects *obj));

/* Commit objects in repo_info's objects list to
 * repository. */
GVM_EXPORT GVM_EXTERN(int gvm_commit_objs,
	       (struct gvm_info *repo_info));


/**
 * GVM model level routines
 */

/* Add a new, empty model to a repository */
GVM_EXPORT GVM_EXTERN(int gvm_new_model,
	       (struct gvm_info *repo_info,
		const char *model_name));

/* Populate repo_info's objects list with the contents of
 * a complete model repository. If ver_num
 * is LATEST_VERSION use latest revision */
GVM_EXPORT GVM_EXTERN(int gvm_get_model,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 size_t ver_num));

/* Populate repo_info's objects list with a  subset of a 
 * model repository. If ver_num is LATEST_VERSION use 
 * latest revision.  If recursive is 1 add all objects
 * below the specified object in the tree. */
GVM_EXPORT GVM_EXTERN(int gvm_get_objs,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 const char *obj_name, 
		 size_t ver_num,
		 int recursive));



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


/* Export a pre-determined subset of a model repository to a .g file. 
 * This function can be used when repo_info's objects list has been
 * populated by custom functions. */
GVM_EXPORT GVM_EXTERN(int gvm_export_list,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 const char *g_file));


/* Export a subset of a model repository to a .g file. If ver_num
 * is LATEST_VERSION use latest revision.  If recursive is 1 
 * recursivly include any objects int the tree below the specified
 * object. */
GVM_EXPORT GVM_EXTERN(int gvm_export_object,
		(struct gvm_info *repo_info,
		 const char *model_name, 
		 const char *obj_name, 
		 const char *g_file,
		 size_t ver_num,
		 int recursive));


