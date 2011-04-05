/*                      G V M _ S V N . H
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
/** @file gvm_svn.h
 *
 * Subversion specific structures and definitions for GVM.
 *  
 */

#include <apr.h>
#include <apr_file_io.h>
#include <apr_signal.h>
#include <apr_hash.h>
#include <apr_tables.h>

#include "svn_pools.h"
#include "svn_path.h"
#include "svn_fs.h"
#include "svn_diff.h"
#include "svn_repos.h"


/**
 * GVMSVN_ERR(expr)
 *
 * We need to handle subversion errors, but don't assume a return
 */
#define GVMSVN_ERR(expr)				\
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
 * SVN Repository info
 *
 * This structure holds all state information pertaining to a
 * subversion geometry repository
 */

struct geomsvn_info {
   apr_pool_t *pool;			/* Apache Portable Runtime memory pool */
   apr_allocator_t *allocator;		/* Apache Portable Runtime allocator */
   apr_pool_t *commit_pool;		/* Subpool for commits */
   svn_repos_t *repos;			/* svn_repos_t pointer to repository */
   svn_revnum_t *curr_rev;		/* Current repository revision (cast to svn_revnum_t to use)*/
};

