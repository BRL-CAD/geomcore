/*                           M E M . C
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
/** @file mem.c
 *
 * Brief description
 *
 */

#include "gvm.h"
#include "gvm_svn.h"

void
gvm_info_init(struct gvm_info *repo_info) {

    struct geomsvn_info *internal;

    /* initialize subversion */
    svn_cmdline_init("libgvm", stderr);

    /* init repository objects list */
    BU_GETSTRUCT(repo_info->objects, repository_objects);
    BU_LIST_INIT(&(repo_info->objects->l));

    /* initialize internal subversion memory pools and info */
    repo_info->internal = bu_malloc(sizeof(struct geomsvn_info), "gvn_info internal structure");
    internal = (struct geomsvn_info *)repo_info->internal;
    apr_allocator_create(&(internal->allocator));
    apr_allocator_max_free_set(internal->allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
    internal->pool = svn_pool_create_ex(NULL, internal->allocator);
    apr_allocator_owner_set(internal->allocator, internal->pool);
    internal->objects_pool = svn_pool_create(internal->pool);

    /* Initialize the FS library. */
    svn_fs_initialize(internal->pool);
}

void
gvm_info_clear_objects(struct gvm_info *repo_info) {
    /* free repository objects list */
    struct geomsvn_info *internal;
    struct repository_objects *entry;
    internal = (struct geomsvn_info *)repo_info->internal;
    while (BU_LIST_WHILE(entry, repository_objects, &(repo_info->objects->l))) {
	BU_LIST_DEQUEUE(&(entry->l));
    }
    svn_pool_clear(internal->objects_pool);
}

void
gvm_info_free(struct gvm_info *repo_info) {

    struct geomsvn_info *internal;

    /* free repository objects list */
    gvm_info_clear_objects(repo_info);
    bu_free(repo_info->objects, "free repo_info->objects");

    /* free subversion memory pools and info */
    internal = (struct geomsvn_info *)repo_info->internal;
    svn_pool_destroy(internal->objects_pool);
    svn_pool_destroy(internal->pool);
    apr_allocator_destroy(internal->allocator);
    bu_free(repo_info->internal, "free gvm_info internal structure");
    bu_free(repo_info, "free gvm_info");
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
