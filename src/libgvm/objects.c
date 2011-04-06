#include "gvm.h"
#include "gvm_svn.h"

int gvm_object_in_model(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num) {
	int ret = 1;
	apr_pool_t *subpool;
	svn_fs_root_t *repo_root;
	svn_node_kind_t *status;
	svn_fs_t *fs;
	struct geomsvn_info *internal = NULL;
	internal = (struct geomsvn_info *)repo_info->internal;
	subpool = svn_pool_create(internal->pool);
	fs = svn_repos_fs(internal->repos);
	svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);

	svn_fs_check_path(status, repo_root, svn_file->data, subpool);
	if (status == svn_node_none) ret = 0;
	svn_pool_destroy(subpool);
	return ret;
}


struct bu_external * gvm_get_extern_obj(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num) {
	apr_pool_t *subpool;
	svn_filesize_t buflen;
	svn_stream_t *obj_contents;
	svn_fs_root_t *repo_root;
	svn_fs_t *fs;
	struct bu_external *obj_extern;
	struct geomsvn_info *internal = NULL;
	if (gvm_object_in_model(repo_info, model_name, obj_name, ver_num)) {
		internal = (struct geomsvn_info *)repo_info->internal;
		subpool = svn_pool_create(internal->pool);
		obj_extern = apr_palloc(internal->objects_pool, sizeof(struct bu_external));
		fs = svn_repos_fs(internal->repos);
		svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
		svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);

		BU_INIT_EXTERNAL(obj_extern);
		svn_fs_file_length(&buflen, repo_root, svn_file->data, subpool);
		obj_extern->ext_nbytes = (size_t)buflen;
		obj_extern->ext_buf = apr_palloc(internal->objects_pool, obj_extern->ext_nbytes);
		svn_fs_file_contents(&obj_contents, repo_root, svn_file->data, subpool);
		svn_stream_read(obj_contents, (char *)obj_extern->ext_buf, (apr_size_t *)&buflen);
		svn_stream_close(obj_contents);
		svn_pool_destroy(subpool);
		return obj_extern;
	} else {
		return NULL;
	}
}

struct repository_objects * gvm_get_repo_obj(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num) {
	struct repository_objects *new_obj;
	struct geomsvn_info *internal =  (struct geomsvn_info *)repo_info->internal;
	new_obj = apr_palloc(internal->objects_pool, sizeof(struct repository_objects));
	new_obj->model_name = apr_pstrdup(internal->objects_pool, model_name);
	new_obj->obj_name = apr_pstrdup(internal->objects_pool, obj_name);
	new_obj->version = ver_num;
	return new_obj;
}
