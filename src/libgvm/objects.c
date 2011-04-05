#include "gvm.h"
#include "gvm_svn.h"

struct repository_objects * gvm_get_repo_obj(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num) {
	apr_pool_t *subpool;
	svn_filesize_t buflen;
	svn_stream_t *obj_contents;
	svn_fs_root_t *repo_root;
	svn_fs_t *fs;
	struct repository_objects *new_obj;
	struct geomsvn_info *internal = NULL;
	internal = (struct geomsvn_info *)repo_info->internal;
	subpool = svn_pool_create(internal->pool);
	new_obj = apr_palloc(internal->objects_pool, sizeof(struct repository_objects));
	new_obj->contents = apr_palloc(internal->objects_pool, sizeof(struct bu_external));
	BU_INIT_EXTERNAL(new_obj->contents);
	fs = svn_repos_fs(internal->repos);
	svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);
	svn_fs_file_length(&buflen, repo_root, svn_file->data, subpool);
	new_obj->contents->ext_nbytes = (size_t)buflen;
	new_obj->contents->ext_buf = apr_palloc(internal->objects_pool, new_obj->contents->ext_nbytes);
	svn_fs_file_contents(&obj_contents, repo_root, svn_file->data, subpool);
	svn_stream_read(obj_contents, (char *)new_obj->contents->ext_buf, (apr_size_t *)&buflen);
	svn_stream_close(obj_contents);
	svn_pool_destroy(subpool);
	BU_LIST_PUSH(&(repo_info->objects->l), &(new_obj->l));
}


