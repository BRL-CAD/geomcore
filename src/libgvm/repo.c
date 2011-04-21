/*                          R E P O . C
 * libgvm
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory
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
/** @file repo.c
 *
 * Brief description
 *
 */

#include "gvm.h"
#include "gvm_svn.h"

int
gvm_init_repo(struct gvm_info *repo_info, const char *repo_path) {
    struct geomsvn_info *internal = NULL;
    if (!repo_info || !repo_path) return -2;
    internal = (struct geomsvn_info *)repo_info->internal;
    if (!internal) gvm_info_init(repo_info);
    repo_info->repo_full_path = svn_path_canonicalize(repo_path, internal->pool);
    if (svn_repos_find_root_path(repo_info->repo_full_path, internal->pool))
	return -1;
    else
	svn_repos_create(&(internal->repos), repo_info->repo_full_path, NULL, NULL, NULL, NULL, internal->pool);
    return 0;
}

int
gvm_open_repo(struct gvm_info *repo_info, const char *repo_path) {
    struct geomsvn_info *internal = NULL;
    if (!repo_info || !repo_path) return -2;
    internal = (struct geomsvn_info *)repo_info->internal;
    if (!internal) gvm_info_init(repo_info);
    repo_info->repo_full_path = svn_path_canonicalize(repo_path, internal->pool);
    if (svn_repos_find_root_path(repo_info->repo_full_path, internal->pool))
	svn_repos_open(&(internal->repos), repo_info->repo_full_path, internal->pool);
    else
	return -1;
    return 0;
}


/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
