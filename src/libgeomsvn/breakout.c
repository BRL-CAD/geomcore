/*                     B R E A K O U T . C 
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
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
/** @file breakout.c
 *
 *  Take a .g database and assign all of its individual components
 *  to a repository.
 */

/* TODO - need a vesion of SVN_INT_ERROR that does what we want in a library 
 * - probably should not be returning something like EXIT_FAILURE*/

/* This function deals with the special case of a .g file that does not
 * exist in the repository and needs all of its contents imported, without
 * any diff logic being applied. This function will not update an existing
 * .g model in the repository with new contents - for that use 
 * geomsvn_import_g_file */
int geomsvn_init_g_file(apr_pool_t *pool, const char *g_file, const char *repo_path) {
	/* We're going to need an apr pool - if one was passed in, make the subpool
	 * using it - otherwise start from scratch */
	apr_pool_t *subpool;
	apr_allocator_t *allocator;
	if (pool) {
		subpool = svn_pool_create(pool);
	} else {
		apr_allocator_create(&allocator);
		apr_allocator_max_free_set(allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
		subpool = svn_subpool_create_ex(NULL, allocator);
		apr_allocator_owner_set(allocator, subpool);
	}
	/* Now that we have a pool, set up the repository */
	{
	 svn_repos_t *repos;
	 svn_fs_t *fs;
	 svn_revnum_t youngest_rev;
	 svn_fs_txn_t *txn;
	 svn_fs_root_t *txn_root;
	 const char *repo_full_path = svn_path_canonicalize(repo_path, pool);
	 if(!svn_repos_find_root_path(repo_full_path, pool)){
		 printf("Repository %s does not exist - use geomsvn_init_repo to create it.\n", repo_full_path);
		 svn_pool_destroy(subpool);
		 return -1;
	 } else {
		 const char *model_path = svn_path_canonicalize(g_file, pool);
		 const char *model_name = svn_path_basename(model_path, pool);
		 svn_repos_open(&repos, repo_full_path, pool);
		 fs = svn_repos_fs(repos);
		 svn_fs_youngest_rev(&youngest_rev, fs, pool);

	 }

