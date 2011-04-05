#include "gvm.h"
#include "gvm_svn.h"

void gvm_info_init(struct gvm_info *repo_info) {

	struct geomsvn_info *internal;

	/* first, get repo_info memory */
	repo_info = bu_malloc(sizeof(struct gvm_info), "gvn_info structure");

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

	/* Initialize the FS library. */
	svn_fs_initialize(internal->pool);
}

void gvm_info_clear_objects(struct gvm_info *repo_info) {
	/* free repository objects list */
	struct geomsvn_info *internal;
	struct repository_objects *entry;
	internal = (struct geomsvn_info *)repo_info->internal;
	while (BU_LIST_WHILE(entry, repository_objects, &(repo_info->objects->l))) {
		BU_LIST_DEQUEUE(&(entry->l));
	}
	svn_pool_clear(internal->objects_pool);
}

void gvm_info_free(struct gvm_info *repo_info) {

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


