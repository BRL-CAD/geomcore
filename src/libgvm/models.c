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


