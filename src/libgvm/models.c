#include "svn_props.h"
#include "gvm.h"
#include "gvm_svn.h"

int gvm_new_model(struct gvm_info *repo_info, const char *model_name) {
	int ret = 0;
	apr_pool_t *subpool;
	svn_fs_root_t *repo_root;
	svn_node_kind_t *status;
	svn_fs_t *fs;
	svn_revnum_t rev;
	svn_fs_txn_t *txn;
	svn_fs_root_t *txn_root;
	svn_string_t *logmsg;
	struct geomsvn_info *internal = NULL;
	internal = (struct geomsvn_info *)repo_info->internal;
	subpool = svn_pool_create(internal->pool);
	apr_hash_t *revprop_table = apr_hash_make(subpool);
	fs = svn_repos_fs(internal->repos);
	svn_fs_youngest_rev(&rev, fs, subpool);
	svn_fs_revision_root(&repo_root, fs, rev, subpool);
	svn_fs_check_path(status, repo_root, model_name, subpool);
	if (status == svn_node_none) {
		logmsg = svn_string_createf(subpool, "Initial creation of model %s", model_name);
		apr_hash_set(revprop_table, SVN_PROP_REVISION_LOG, APR_HASH_KEY_STRING, logmsg);
		svn_repos_fs_begin_txn_for_commit2(&txn, internal->repos, rev, revprop_table, subpool);
		svn_fs_txn_root(&txn_root, txn, subpool);
		svn_fs_make_dir(txn_root, model_name, subpool);
		svn_repos_fs_commit_txn(NULL, internal->repos, &rev, txn, subpool);
	} else {
		ret = 1;
	}
	svn_pool_destroy(subpool);
	return ret;
}

int gvm_get_model(struct gvm_info *repo_info, const char *model_name, size_t ver_num) {
	struct geomsvn_info *internal = NULL;
	internal = (struct geomsvn_info *)repo_info->internal;
	apr_pool_t *subpool = svn_pool_create(internal->pool);
	apr_hash_t *objects = apr_hash_make(subpool);
	apr_hash_index_t *obj;
	const void *key;
	apr_ssize_t klen;
	svn_fs_t *fs = svn_repos_fs(internal->repos);
	svn_fs_root_t *repo_root;
	struct repository_objects *new_obj;
	svn_fs_revision_root(&repo_root, fs, (svn_revnum_t)ver_num, subpool);
	svn_fs_dir_entries(&objects, repo_root, model_name, subpool);
	for (obj = apr_hash_first(subpool, objects); obj; obj = apr_hash_next(obj)) {
		apr_hash_this(obj, &key, &klen, NULL);
		new_obj = gvm_get_repo_obj(repo_info, model_name, (const char *)key, ver_num);
		new_obj->contents = gvm_get_extern_obj(repo_info, model_name, (const char *)key, ver_num);
		gvm_add_to_list(repo_info, new_obj);
	}
	svn_pool_destroy(subpool);
}



int gvm_get_objs(struct gvm_info *repo_info, const char *model_name, const char *obj_name, size_t ver_num, int recursive) {
	struct geomsvn_info *internal = NULL;
	internal = (struct geomsvn_info *)repo_info->internal;
	apr_pool_t *subpool = svn_pool_create(internal->pool);
	apr_hash_t *objects = apr_hash_make(subpool);
	apr_hash_t *todo= apr_hash_make(subpool);
	apr_hash_t *tmp = apr_hash_make(subpool);
	apr_hash_t *tmp2;
	apr_hash_index_t *obj;
	const void *key;
	const void *val;
	apr_ssize_t klen;
	svn_string_t *objname;
	struct bu_external *contents;
	/* First, get the requested object */
	if (recursive) {
		apr_hash_set(todo, (const void *)obj_name, APR_HASH_KEY_STRING, (const void *)contents);
	} else {
		contents = gvm_get_extern_obj(repo_info, model_name, obj_name, ver_num);
		apr_hash_set(objects, (const void *)obj_name, APR_HASH_KEY_STRING, (const void *)contents);
	}
	/* If we aren't recursive, we're done.  If we are, we need to pull the list of
	 * children of that comb and follow the tree down */
	/* TODO - in-mem dbi so we can go from external to internal, get the comb list (if any) and
	 * act on it.  initially put entries found in the todo hash.  if they're present in the 
	 * objects hash, set to null - otherwise, process them and set to null.  When apr_hash_count
	 * is 0, we're done.*/
	while (apr_hash_count(todo)) {
		for (obj = apr_hash_first(subpool, todo); obj; obj = apr_hash_next(obj)) {
			apr_hash_this(obj, &key, &klen, &val);
			if (!apr_hash_get(objects, (const void *)key, APR_HASH_KEY_STRING)) {
				contents = gvm_get_extern_obj(repo_info, model_name, (const char *)key, ver_num);
				apr_hash_set(objects, (const void *)key, APR_HASH_KEY_STRING, (const void *)contents);
				/* contents to internal, get comb list, add to tmp */
			}
			apr_hash_set(todo, (const void *)key, APR_HASH_KEY_STRING, NULL);
		}
		tmp2 = todo;
		todo = tmp;
		tmp = tmp2;
	}

}
