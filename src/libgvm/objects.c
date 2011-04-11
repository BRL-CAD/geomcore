#include "gvm.h"
#include "gvm_svn.h"

int gvm_object_in_model(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num) {
	int ret = 1;
	struct geomsvn_info *internal = (struct geomsvn_info *)repo_info->internal;
	apr_pool_t *subpool = svn_pool_create(internal->pool);
	svn_fs_root_t *repo_root;
	svn_node_kind_t status;
	svn_fs_t *fs = svn_repos_fs(internal->repos);;
	svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);

	svn_fs_check_path(&status, repo_root, svn_file->data, subpool);
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
	internal = (struct geomsvn_info *)repo_info->internal;
	subpool = svn_pool_create(internal->pool);
	obj_extern = apr_palloc(internal->objects_pool, sizeof(struct bu_external));
	fs = svn_repos_fs(internal->repos);
	svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);
	if (gvm_object_in_model(repo_info, model_name, obj_name, ver_num)) {
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
		svn_pool_destroy(subpool);
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

int gvm_diff(struct gvm_info *repo_info, struct bu_external *obj1, struct bu_external *obj2) {
	if (!obj1 && !obj2) return 0;
	if (!obj1 || !obj2) return 1;
	if (obj1->ext_nbytes != obj2->ext_nbytes) return 1;
	if (memcmp(obj1->ext_buf, obj2->ext_buf, obj1->ext_nbytes) != 0) return 1;
	return 0;
}

int gvm_add_to_list(struct gvm_info *repo_info, struct repository_objects *obj) {
	BU_LIST_PUSH(&(repo_info->objects->l), &(obj->l));
}

int gvm_commit_objs(struct gvm_info *repo_info) {
	struct geomsvn_info *internal = (struct geomsvn_info *)repo_info->internal;
	apr_pool_t *subpool = svn_pool_create(internal->pool);
	void *edit_baton, *root_baton, *file_baton, *handler_baton, *child_baton;
	svn_txdelta_window_handler_t handler;
	struct repository_objects *obj;
	svn_string_t *target_dir;
	svn_string_t *target_file;
	svn_stringbuf_t *contents;
	svn_stream_t *contents_stream;
	svn_fs_t *fs;
	svn_revnum_t rev;
	const svn_delta_editor_t **editor = apr_palloc(subpool, sizeof(struct svn_delta_editor_t));
	subpool = svn_pool_create(internal->pool);
	fs = svn_repos_fs(internal->repos);
	svn_fs_youngest_rev(&rev, fs, subpool);
	svn_repos_get_commit_editor4(editor, &edit_baton, internal->repos, NULL, repo_info->repo_full_path, "/", NULL, NULL, NULL, NULL, NULL, NULL, subpool);
	(*editor)->open_root(edit_baton, rev, subpool, &root_baton);
	for(BU_LIST_FOR(obj, repository_objects , &(repo_info->objects->l))) {
		target_dir = svn_string_createf(subpool, "%s/%s", obj->model_name, obj->obj_name);
		target_file = svn_string_createf(subpool, "%s/%s/%s", obj->model_name, obj->obj_name, obj->obj_name);
		switch(obj->action) {
			case 1: /* update */
				(*editor)->open_file(target_file->data, root_baton, obj->version, subpool, &file_baton);
				(*editor)->apply_textdelta(file_baton, NULL, subpool, &handler, &handler_baton);
				contents = svn_stringbuf_ncreate((const char *)obj->contents->ext_buf, (apr_size_t)obj->contents->ext_nbytes, subpool);
				contents_stream = svn_stream_from_stringbuf(contents, subpool);
				svn_txdelta_send_stream(contents_stream, handler, handler_baton, NULL, subpool);
				svn_stream_close(contents_stream);
				(*editor)->close_file(file_baton, NULL, subpool);
				break;
			case 2: /* add */
				(*editor)->add_directory(target_dir->data,  root_baton, NULL, rev, subpool, &child_baton);
				(*editor)->add_file(target_file->data, root_baton, NULL, rev, subpool, &file_baton);
				(*editor)->apply_textdelta(file_baton, NULL, subpool, &handler, &handler_baton);
				contents = svn_stringbuf_ncreate((const char *)obj->contents->ext_buf, (apr_size_t)obj->contents->ext_nbytes, subpool);
				contents_stream = svn_stream_from_stringbuf(contents, subpool);
				svn_txdelta_send_stream(contents_stream, handler, handler_baton, NULL, subpool);
				svn_stream_close(contents_stream);
				(*editor)->close_file(file_baton, NULL, subpool);
				break;
			case 3: /* delete */
				(*editor)->delete_entry(target_dir->data, obj->version, root_baton, subpool);
				break;
		}
	}
	(*editor)->close_edit(edit_baton, subpool);
	gvm_info_clear_objects(repo_info);
	svn_pool_destroy(subpool);
}
