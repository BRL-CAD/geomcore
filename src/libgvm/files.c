#include "svn_props.h"
#include "raytrace.h"
#include "gvm.h"
#include "gvm_svn.h"

int gvm_import_g_file(struct gvm_info *repo_info, const char *g_file) {
	int ret = 0;
	int inc;
	apr_pool_t *subpool, *iterpool;
	svn_fs_root_t *repo_root;
	svn_node_kind_t *status;
	svn_fs_t *fs;
	svn_revnum_t rev;
	svn_fs_txn_t *txn;
	svn_fs_root_t *txn_root;
	svn_string_t *logmsg, *filedir, *filepath;
	svn_stream_t *rt_data_stream;
	struct geomsvn_info *internal = NULL;
	
	struct db_i *dbip = DBI_NULL;
	struct rt_wdb *wdbp = RT_WDB_NULL;
	struct directory *dp;

	internal = (struct geomsvn_info *)repo_info->internal;
	subpool = svn_pool_create(internal->pool);
	iterpool = svn_pool_create(internal->pool);

	const char *model_path = svn_path_canonicalize(g_file, subpool);
	const char *model_name = svn_path_basename(model_path, subpool);
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

		dbip = db_open(model_path, "r");
		if(dbip == DBI_NULL) {
			ret = 2;
		} else {
			(void)db_dirbuild(dbip);
			wdbp = wdb_dbopen(dbip, RT_WDB_TYPE_DB_DISK);
			db_update_nref(dbip, &rt_uniresource);
			struct bu_external *data = apr_palloc(subpool, sizeof(struct bu_external));
			struct rt_db_internal *ip = apr_palloc(subpool, sizeof(struct rt_db_internal));
			for (inc=0; inc < RT_DBNHASH; inc++) {
				for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
					if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
						rt_data_stream = svn_stream_empty(iterpool);
						rt_db_get_internal5(ip, dp, dbip, NULL, &rt_uniresource);
						rt_db_cvt_to_external5(data, dp->d_namep, ip, 1, dbip,  &rt_uniresource, ip->idb_major_type);
						filedir = svn_string_createf(iterpool, "%s/%s", model_name, dp->d_namep);
						filepath = svn_string_createf(iterpool, "%s/%s/%s", model_name, dp->d_namep, dp->d_namep);
						svn_fs_make_dir(txn_root, filedir->data, iterpool);
						svn_fs_make_file (txn_root, filepath->data, iterpool);
						svn_fs_apply_text (&rt_data_stream, txn_root, filepath->data, NULL, iterpool);
						svn_stream_write(rt_data_stream, (const char *)data->ext_buf, (apr_size_t *)&data->ext_nbytes);
						svn_stream_close(rt_data_stream);
					}
					svn_pool_clear(iterpool);
				}
			}
			db_close(dbip);
		}
		svn_repos_fs_commit_txn(NULL, internal->repos, &rev, txn, subpool);
	} else {
		ret = 1;
	}
	svn_pool_destroy(iterpool);
	svn_pool_destroy(subpool);
	return ret;
}

