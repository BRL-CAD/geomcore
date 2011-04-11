#include "svn_props.h"
#include "raytrace.h"
#include "gvm.h"
#include "gvm_svn.h"

int gvm_import_g_file(struct gvm_info *repo_info, const char *g_file) {
	int ret = 0;
	int inc;
	apr_pool_t *subpool, *iterpool;
	svn_fs_root_t *repo_root;
	svn_node_kind_t status;
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

	svn_fs_check_path(&status, repo_root, model_name, subpool);
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

int gvm_export_g_file(struct gvm_info *repo_info, const char *model_name, const char *g_file, size_t ver_num) {
	 int ret = 0;
	 struct geomsvn_info *internal = NULL;
	 apr_pool_t *subpool;
	 internal = (struct geomsvn_info *)repo_info->internal;
	 subpool = svn_pool_create(internal->pool);
	 const char *model_path = svn_path_canonicalize(g_file, subpool);
	 svn_fs_t *fs;
	 svn_fs_root_t *repo_root;
	 svn_node_kind_t status;
	 svn_revnum_t rev;
	 apr_hash_t *objects = apr_hash_make(subpool);
	 apr_hash_index_t *obj;
	 const void *key;
	 apr_ssize_t klen;
	 struct db_i *dbip = DBI_NULL;
	 struct rt_wdb *wdbp = RT_WDB_NULL;
	 struct bu_external *contents;
	 struct rt_db_internal ip;
	 RT_INIT_DB_INTERNAL(&ip);

	 fs = svn_repos_fs(internal->repos);
	 if(ver_num) {
		 rev = (svn_revnum_t)ver_num;
	 } else {
		 svn_fs_youngest_rev(&rev, fs, subpool);
	 }
	 svn_fs_revision_root(&repo_root, fs, rev, subpool);
	 svn_fs_check_path(&status, repo_root, model_name, subpool);
	 if (status != svn_node_dir) {
		 ret = 1;
	 } else {
		 if (!bu_file_exists(model_path)){
			 wdbp = wdb_fopen(model_path);
			 dbip = wdbp->dbip;
			 if (dbip) {
				 svn_fs_dir_entries(&objects, repo_root, model_name, subpool);
				 for (obj = apr_hash_first(subpool, objects); obj; obj = apr_hash_next(obj)) {
					 apr_hash_this(obj, &key, &klen, NULL);
					 contents = gvm_get_extern_obj(repo_info, model_name, (const char *)key, (size_t)rev);
					 if (contents) {
						 rt_db_external5_to_internal5(&ip, contents, (const char *)key, dbip, NULL, &rt_uniresource);
						 wdb_put_internal(wdbp, (const char *)key, &ip, 1);
					 }
				 }
				 svn_pool_clear(internal->objects_pool);
				 wdb_close(wdbp);
			 } else {
				 ret = 3;
			 }
		 } else {
			 ret = 2;
		 }
	 }
	svn_pool_destroy(subpool);
	return ret;
}

int gvm_commit_g_file(struct gvm_info *repo_info, const char *g_file) {
	 int ret = 0;
	 int inc;
	 struct geomsvn_info *internal = NULL;
	 apr_pool_t *subpool;
	 internal = (struct geomsvn_info *)repo_info->internal;
	 subpool = svn_pool_create(internal->pool);
	 const char *model_path = svn_path_canonicalize(g_file, subpool);
	 const char *model_name = svn_path_basename(model_path, subpool);
	 svn_fs_t *fs;
	 svn_fs_root_t *repo_root;
	 svn_node_kind_t status;
	 svn_revnum_t rev;
	 apr_hash_t *repo_objects = apr_hash_make(subpool);
	 apr_hash_t *file_objects = apr_hash_make(subpool);
	 apr_hash_index_t *obj;
	 const void *key;
	 apr_ssize_t klen;
	 struct repository_objects *new_obj;
	 struct db_i *dbip;
	 struct rt_wdb *wdbp;
	 struct bu_external *repo_contents, *file_contents;
	 struct directory *dp;
	 struct rt_db_internal ip;
	 RT_INIT_DB_INTERNAL(&ip);

	 fs = svn_repos_fs(internal->repos);
	 svn_fs_youngest_rev(&rev, fs, subpool);
	 svn_fs_revision_root(&repo_root, fs, rev, subpool);
	 svn_fs_check_path(&status, repo_root, model_name, subpool);
	 if (status != svn_node_dir) {
		 ret = 1;
	 } else {
		 if (bu_file_exists(model_path)){
			 dbip = db_open(model_path, "r");
			 if(dbip == DBI_NULL) {
				 ret = 3;
			 } else {
				 /* list out repo contents */
				 svn_fs_dir_entries(&repo_objects, repo_root, model_name, subpool);
				 /* list out file contents */
				 for (inc=0; inc < RT_DBNHASH; inc++) {
					 for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
						 if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
							 apr_hash_set(file_objects, (const void *)dp->d_namep, APR_HASH_KEY_STRING, (const void *)dp->d_namep);
						 }
					 }
				 }
				 /* Start sorting out what changed. */
				 for (obj = apr_hash_first(subpool, repo_objects); obj; obj = apr_hash_next(obj)) {
					 apr_hash_this(obj, &key, &klen, NULL);
					 /* If the repo object isn't in file, add a delete object */
					 if (!apr_hash_get(file_objects, key, APR_HASH_KEY_STRING)) {
						 new_obj = gvm_get_repo_obj(repo_info, model_name, (const char *)key, rev);
						 new_obj->action = 3;
						 gvm_add_to_list(repo_info, new_obj);
					 } else {
					 /* if it is in repo, check diff */
						 repo_contents = gvm_get_extern_obj(repo_info, model_name, (const char *)key, rev);
						 dp = db_lookup(dbip, (const char *)key, LOOKUP_QUIET);
						 rt_db_get_internal5(&ip, dp, dbip, NULL, &rt_uniresource);
						 rt_db_cvt_to_external5(file_contents, dp->d_namep, &ip, 1, dbip,  &rt_uniresource, ip.idb_major_type);
						 if (gvm_diff(repo_info, file_contents, repo_contents)) {
							 new_obj = gvm_get_repo_obj(repo_info, model_name, (const char *)key, rev);
							 new_obj->contents = file_contents;
							 new_obj->action = 1;
						 }
					 }
					 apr_hash_set(file_objects, key, APR_HASH_KEY_STRING, NULL);
				 }
				 /* Anything not already dealt with is a new file */
				 for (obj = apr_hash_first(subpool, file_objects); obj; obj = apr_hash_next(obj)) {
					 dp = db_lookup(dbip, (const char *)key, LOOKUP_QUIET);
					 rt_db_get_internal5(&ip, dp, dbip, NULL, &rt_uniresource);
					 rt_db_cvt_to_external5(file_contents, dp->d_namep, &ip, 1, dbip,  &rt_uniresource, ip.idb_major_type);
					 new_obj = gvm_get_repo_obj(repo_info, model_name, (const char *)key, rev);
					 new_obj->contents = file_contents;
					 new_obj->action = 2;
				 }
				 gvm_commit_objs(repo_info);
			 }
		 } else {
			 ret = 2;
		 }
	 }
}
	

