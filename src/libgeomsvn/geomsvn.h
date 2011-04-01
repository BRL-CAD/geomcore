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


/**
 * List of items for committing to a repository
 */
struct commit_items {
   struct bu_list l;
   char *obj_name;
   struct bu_external *contents;
}

/**
 * Repository Structure
 *
 * This structure holds all state information pertaining to a
 * geometry repository
 */

struct geomsvn_info {
   void *pool;			/* Apache Portable Runtime memory pool */
   void *commit_pool;		/* Subpool for commits */
   char *repo_full_path;	/* Full filesystem path to svn repository */
   char *svn_file_full_path;	/* Full svn_fs path from repository root to working file (ktank.g/object1.s/object1.s)*/
   char *svn_file;		/* svn_fs path of working file from the toplevel model dir (object1.s/object1.s)*/
   char *model_name;		/* Name of .g model (e.g. ktank.g) */
   char *obj_name;		/* Name of individual file (e.g. object1.s) */
   void *repos;			/* svn_repos_t pointer to repository */
   size_t *curr_rev;		/* Current repository revision (cast to svn_revnum_t to use)*/
}

/**
 * Utility routines for repository structures
 */
 
GSVN_EXPORT GSVN_EXTERN(void geomsvn_info_init,
		(struct geomsvn_info *repo_info));
  



/**
 * GSVN Repository Functions
 */

GSVN_EXPORT GSVN_EXTERN(int geomsvn_init_repo,
		(struct geomsvn_info *repo_info, 
		 const char *repo_path));

GSVN_EXTERN(svn_repo

