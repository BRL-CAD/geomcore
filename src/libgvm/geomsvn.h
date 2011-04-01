/*                      G E O M S V N . H
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
/** @file geomsvn.h
 *
 * Wrapper library that encapsulates subversion functionality for use
 * with geometry databases.
 *  
 */

#ifndef GSVN_EXPORT
#  if defined(_WIN32) && !defined(__CYGWIN__) && defined(BRLCAD_DLL)
#    ifdef GSVN_EXPORT_DLL
#      define GSVN_EXPORT __declspec(dllexport)
#    else
#      define GSVN_EXPORT __declspec(dllimport)
#    endif
#  else
#    define GSVN_EXPORT
#  endif
#endif

#define GSVN_EXTERN(type_and_name, args) extern type_and_name args

/**
 * GEOMSVN_ERR(expr)
 *
 * We need to handle subversion errors, but don't assume a return
 */
#define GSVN_ERR(expr)				\
	do {						\
		svn_error_t *svn_err = (expr);    	\
		if (svn_err) {				\
		   if (SVN_DEBUG)			\
		      svn_handle_error2(svn_err, stderr, FALSE, "svn: ");  \
		   ret = svn_err->apr_err;		\
		   svn_error_clear(svn_err);		\
		}					\
	} while (0)


/* Convenient definitions */
#define SVN_HEAD -1


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
   int rev;
   struct bu_external *contents;
   int action; /* 0 = update, 1 = add, 2 = delete  - used for commit operations*/
}


/**
 * Repository Structure
 *
 * This structure holds all state information pertaining to a
 * geometry repository
 */

struct geomsvn_info {
   void *pool;				/* Apache Portable Runtime memory pool */
   void *commit_pool;			/* Subpool for commits */
   char *repo_full_path;		/* Full filesystem path to svn repository */
   void *repos;				/* svn_repos_t pointer to repository */
   struct repository_objects *objects;	/* List of currently "active" items */
   char *user;				/* User ID */
   int *curr_rev;			/* Current repository revision (cast to svn_revnum_t to use)*/
}

/**
 * Utility routines for repository structures
 */

/* Initialize APR pool and repository_items list */
GSVN_EXPORT GSVN_EXTERN(void geomsvn_info_init,
		(struct geomsvn_info *repo_info));
  
/* Free all items in the geomsvn_info structure */ 
GSVN_EXPORT GSVN_EXTERN(void geomsvn_info_free,
		(struct geomsvn_info *repo_info));

/* Clear repository items list */
GSVN_EXPORT GSVN_EXTERN(void geomsvn_info_clear_items,
		(struct geomsvn_info *repo_info));


/**
 * GSVN Repository Functions
 */

/* Create a repository at the filesystem location repo_path */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_init_repo,
		(struct geomsvn_info *repo_info, 
		 const char *repo_path));

/* Fill in the repo_full_path, repos pointer and the user name. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_open_repo,
		(struct geomsvn_info *repo_info, 
		 const char *repo_path,
		 const char *user));

/**
 * GSVN .g file helper routines
 */ 

/* Populate a new model repository using a .g file. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_import_g_file,
		(struct geomsvn_info *repo_info, 
		 const char *g_file));

/* List objects present in a .g file but not in the corresponding
 * model repository. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_g_file_get_add_list,
		(struct geomsvn_info *repo_info, 
		 const char *g_file));

/* List objects present in a model repository but not in the 
 * corresponding .g file. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_g_file_get_delete_list,
		(struct geomsvn_info *repo_info, 
		 const char *g_file));

/* List objects present in both a model repository and in the 
 * corresponding .g file that differ. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_g_file_get_diff_list,
		(struct geomsvn_info *repo_info, 
		 const char *g_file));

/* Update an existing model repository using a .g file. */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_commit_g_file,
		(struct geomsvn_info *repo_info, 
		 const char *g_file));

/* Export a complete model repository to a .g file. If revnum
 * is SVN_HEAD use latest revision */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_export_g_file,
		(struct geomsvn_info *repo_info,
		 const char *model_name, 
		 const char *g_file,
		 int revnum));

/* Export a subset of a model repository to a .g file. If revnum
 * is SVN_HEAD use latest revision */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_export_object,
		(struct geomsvn_info *repo_info,
		 const char *model_name, 
		 const char *obj_name, 
		 const char *g_file,
		 int revnum,
		 int recursive));

/**
 * GSVN object level routines
 */

/* Populate a repository_objects struct with the contents
 * of obj_name.  This is how to go from svn's internal
 * stored copy of an object to the bu_external needed for
 * BRL-CAD .g level operations.*/
GSVN_EXPORT GSVN_EXTERN(struct repository_objects * geomsvn_get_repo_obj,
		(struct geomsvn_info *repo_info,
		 const char *model_name,
		 const char *obj_name,
		 int revnum));

/* Check if an object exists in a given model as defined
 * in revision revnum.  If revnum is SVN_HEAD use
 * latest revision */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_object_in_model,
	       (struct geomsvn_info *repo_info,
		const char *model_name,
		const char *obj_name,
		int revnum));

/* Very simple routine to determine if two objects
 * are different - based on binary data, not geometry
 * aware.  Returns 0 if they're the same and one if
 * they differ */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_diff,
	       (struct geomsvn_info *repo_info,
		struct repository_objects *obj1,
		struct repository_objects *obj2));


/* Add a repository_objects struct to repo_info's
 * objects list */
GSVN_EXPORT GSVN_EXTERN(int geomsvn_add_to_list,
	       (struct geomsvn_info *repo_info,
		struct repository_objects *obj));


	
