#include "common.h"

#include <time.h>
#include <sys/stat.h>

#include <apr.h>
#include <apr_file_io.h>
#include <apr_signal.h>
#include <apr_hash.h>
#include <apr_tables.h>

#include "svn_pools.h"
#include "svn_path.h"
#include "svn_fs.h"
#include "svn_diff.h"
#include "svn_repos.h"

#include "bu.h"
#include "vmath.h"
#include "bn.h"
#include "dg.h"
#include "mater.h"
#include "libtermio.h"
#include "db.h"
#include "raytrace.h"
#include "ged.h"

struct assemble_info {
	apr_pool_t *pool;
	struct bu_vls svn_file;
	const char *model_file;
	struct db_i *dbip;
	struct rt_wdb *wdbp;
	const char *model_name;
	svn_fs_root_t *root;
};

/* Convert a file from an svn repository into a bu_external structure.  Caller is responsible for
 * freeing both the ext_buf memory and the bu_external struct itself */
struct bu_external *svn_file_to_bu_extern(apr_pool_t *pool, svn_fs_root_t *repo_root, const char *model_name, const char *obj_name) {
	apr_pool_t *subpool = svn_pool_create(pool);
	svn_filesize_t buflen;
	svn_stream_t *obj_contents;
	struct bu_external *data = bu_malloc(sizeof(struct bu_external), "allocate bu_external");
	BU_INIT_EXTERNAL(data);
	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);
	svn_string_t *target_file = svn_string_createf(subpool, "%s/%s", obj_name, obj_name);
	svn_fs_file_length(&buflen, repo_root, svn_file->data, subpool);
	data->ext_nbytes = (size_t)buflen;
	data->ext_buf = bu_malloc(data->ext_nbytes, "memory for .g data");
	svn_fs_file_contents(&obj_contents, repo_root, svn_file->data, subpool);
	svn_stream_read(obj_contents, (char *)data->ext_buf, (apr_size_t *)&buflen);
	svn_stream_close(obj_contents);
	svn_pool_destroy(subpool);
	return data;
}

/* Function callback to assemble .g file - function type is
 * apr_hash_do_callback_fn_t.  Need to make sure dbinfo is
 * a struct that has everything we need to get the file
 * contents (probably with svn_ra_get_file).  Take the stream
 * and convert it to a svn_string_t with svn_string_from_stream.
 * That stream should then be convertable to a bu_external
 * structure and converted to a form to insert into the db*/
int concat_obj(void *dbinfo, const void *objname, apr_ssize_t klen, const void *objsvninfo)
{
  struct assemble_info *ainfo = (struct assemble_info *)dbinfo;
  apr_pool_t *subpool = svn_pool_create(ainfo->pool);
  struct directory *dp;
  struct rt_db_internal ip;
  struct bu_external *data;

  RT_INIT_DB_INTERNAL(&ip);

  data = svn_file_to_bu_extern(subpool, ainfo->root, ainfo->model_name, objname);

  /* Put things into the new database */
  rt_db_external5_to_internal5(&ip, data, (const char *)objname, ainfo->dbip, NULL, &rt_uniresource);
  wdb_put_internal(ainfo->wdbp, (const char *)objname, &ip, 1);

  svn_pool_destroy(subpool);
  bu_free(data->ext_buf, "free ext buf data");
  bu_free(data, "free ext buf");
}

struct commit_items {
	struct bu_list l;
	char *obj_name;
	struct bu_external *contents;
};

/* Function to determine if a .g object has been changed */
int check_obj(apr_pool_t *pool, struct commit_items *update_list, svn_repos_t *repos, svn_revnum_t rev, const char *model_name, const char *obj_name, struct bu_external *newcontents) {
	int altered_geom = 0;
	svn_fs_t *fs = svn_repos_fs(repos);
	svn_fs_root_t *repo_root;
	svn_filesize_t buflen;
	struct bu_external *data;
	apr_pool_t *subpool = svn_pool_create(pool);
	svn_fs_revision_root(&repo_root, fs, rev, subpool);
	struct commit_items *commit_item;
  	svn_string_t *svn_file = svn_string_createf(subpool, "%s/%s/%s", model_name, obj_name, obj_name);
	svn_fs_file_length(&buflen, repo_root, svn_file->data, subpool);
  	if ((size_t)buflen != (size_t)newcontents->ext_nbytes) {
		printf("found size difference\n");
		altered_geom = 1;
	} else {
		/* sizes are the same, we need to check the contents */
		data = svn_file_to_bu_extern(subpool, repo_root, model_name, obj_name);
		if (memcmp(data->ext_buf, newcontents->ext_buf, (size_t)buflen) != 0) {
			printf("found content difference\n");
			altered_geom = 1;
		}
		bu_free(data->ext_buf, "free ext buf data");
		bu_free(data, "free ext buf");
	}
	if (altered_geom) {
		/* add to updated_list */
		printf("adding: %s\n", obj_name);
		BU_GETSTRUCT(commit_item, commit_items);
		commit_item->obj_name = apr_pstrdup(pool, obj_name);
		commit_item->contents = newcontents;
		BU_LIST_PUSH(&(update_list->l), &(commit_item->l));
	}
	svn_pool_destroy(subpool);
	return altered_geom;
}

/* Function to commit a list of file changes as one commit */
int commit_objs(apr_pool_t *pool, struct commit_items *update_list, svn_repos_t *repos, svn_revnum_t rev, const char *repo_full_path, const char *model_name, const char *user, const char *logmsg) {
	int i = 0;
	const svn_delta_editor_t **editor = bu_malloc(sizeof(svn_delta_editor_t), "delta editor");
	void *edit_baton, *root_baton, *file_baton, *handler_baton;
	svn_txdelta_window_handler_t handler;
	struct commit_items *item;
	svn_string_t *target_file;
	svn_stringbuf_t *contents;
	svn_stream_t *contents_stream;
	apr_pool_t *subpool = svn_pool_create(pool);
	svn_repos_get_commit_editor4(editor, &edit_baton, repos, NULL, repo_full_path, "/", user, logmsg, NULL, NULL, NULL, NULL, subpool);
	(*editor)->open_root(edit_baton, rev, subpool, &root_baton);
	for(BU_LIST_FOR(item, commit_items, &(update_list->l))) {
		printf("updating: %s\n", item->obj_name);
		target_file = svn_string_createf(subpool, "%s/%s/%s", model_name, item->obj_name, item->obj_name);
		(*editor)->open_file(target_file->data, root_baton, rev, subpool, &file_baton);
		(*editor)->apply_textdelta(file_baton, NULL, subpool, &handler, &handler_baton);
		contents = svn_stringbuf_ncreate((const char *)item->contents->ext_buf, (apr_size_t)item->contents->ext_nbytes, subpool);
		contents_stream = svn_stream_from_stringbuf(contents, subpool);
		svn_txdelta_send_stream(contents_stream, handler, handler_baton, NULL, subpool);
		svn_stream_close(contents_stream);
		(*editor)->close_file(file_baton, NULL, subpool);
		i++;
	}
	(*editor)->close_edit(edit_baton, subpool);
	svn_pool_destroy(subpool);
	bu_free(editor, "free svn editor object");
}

/** Main. **/

int
main(int argc, const char *argv[])
{
  svn_error_t *err;
  svn_repos_t *repos;
  svn_fs_t *fs;
  svn_revnum_t youngest_rev;
  svn_fs_txn_t *txn;
  svn_fs_root_t *txn_root;

  apr_status_t apr_err;
  apr_allocator_t *allocator;
  apr_pool_t *pool;
  struct db_i *dbip = DBI_NULL;
  struct rt_wdb *wdbp = RT_WDB_NULL;
  int inc;
  struct bu_vls vstr;

  time_t tb, t0, t1;
  int tdiff;
  tb = time(NULL);
  t0 = time(NULL);
  t1 = time(NULL);

  if (argc < 2) {
	  printf("Please supply .g file for test\n");
	  exit(0);
  }

  svn_cmdline_init("svntest", stderr);

  apr_allocator_create(&allocator);
  apr_allocator_max_free_set(allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
  pool = svn_pool_create_ex(NULL, allocator);
  apr_allocator_owner_set(allocator, pool);

  /* Initialize the FS library. */
  svn_fs_initialize(pool);

  char *repo_path = "./GS_repository";
  const char *repo_full_path = svn_path_canonicalize(repo_path, pool);

  /* Check if it's already created - if it is, don't try to re-create it */
  if(svn_repos_find_root_path(repo_full_path, pool)){
    printf("Repository %s already exists, continuing with test.\n", repo_full_path);
  } else {
      svn_repos_create(&repos, repo_full_path, NULL, NULL, NULL, NULL, pool);
      printf("Created repository: %s\n", repo_full_path);
  }

  svn_repos_open(&repos, repo_full_path, pool);
  fs = svn_repos_fs(repos);
  svn_fs_youngest_rev(&youngest_rev, fs, pool);
  svn_repos_fs_begin_txn_for_commit2(&txn, repos, youngest_rev, apr_hash_make(pool), pool);
  svn_fs_txn_root(&txn_root, txn, pool);

  const char *model_path = svn_path_canonicalize(argv[1], pool);
  const char *model_name = svn_path_basename(model_path, pool);

  svn_fs_make_dir(txn_root, model_name, pool);

  dbip = db_open(model_path, "r");
  if(dbip == DBI_NULL) {
     printf("could not open %s\n", model_path);
     exit(EXIT_FAILURE);
  }
  (void)db_dirbuild(dbip);
  wdbp = wdb_dbopen(dbip, RT_WDB_TYPE_DB_DISK);
  db_update_nref(dbip, &rt_uniresource);

  /* time for initial setup */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("initial setup: %d sec\n", tdiff);
  t0 = time(NULL);

  struct directory *dp;
  struct bu_vls filedir;
  struct bu_vls filepath;
  svn_stream_t *rt_data_stream;
  bu_vls_init(&filedir);
  bu_vls_init(&filepath);

  /* will need to use an iterpool in here: http://www.opensubscriber.com/message/users@subversion.tigris.org/8428443.html */
  struct bu_external *data = bu_malloc(sizeof(struct bu_external), "alloc external data struct");
  struct rt_db_internal *ip = (struct rt_db_internal *) bu_malloc(sizeof(struct rt_db_internal), "allocate structure");
  char *buf = apr_palloc(pool, SVN__STREAM_CHUNK_SIZE);
  for (inc=0; inc < RT_DBNHASH; inc++) {
	  for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
		  if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
			  rt_data_stream = svn_stream_empty(pool);
			  rt_db_get_internal5(ip, dp, dbip, NULL, &rt_uniresource);
			  rt_db_cvt_to_external5(data, dp->d_namep, ip, 1, dbip,  &rt_uniresource, ip->idb_major_type);
			  bu_vls_sprintf(&filedir, "%s/%s", model_name, dp->d_namep);
			  bu_vls_sprintf(&filepath, "%s/%s/%s", model_name, dp->d_namep, dp->d_namep);
			  svn_fs_make_dir(txn_root, bu_vls_addr(&filedir), pool);
			  svn_fs_make_file (txn_root, bu_vls_addr(&filepath), pool);
			  svn_fs_apply_text (&rt_data_stream, txn_root, bu_vls_addr(&filepath), NULL, pool);
			  svn_stream_write(rt_data_stream, (const char *)data->ext_buf, (apr_size_t *)&data->ext_nbytes);
			  svn_stream_close(rt_data_stream);
		  }
	  }
  }
  bu_free(data, "free external data");
  bu_vls_free(&filedir);
  bu_vls_free(&filepath);
  db_close(dbip);

  /* time for breakout and insertion*/
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf(".g breakout and insertion: %d sec\n", tdiff);
  t0 = time(NULL);

  /* Commit the changes */
  svn_repos_fs_commit_txn(NULL, repos, &youngest_rev, txn, pool);

  /* Close the repository */

  /* time for committing files */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("committed files: %d sec\n", tdiff);
  t0 = time(NULL);

  char *user = "testuser";
  char *logmsg = "testlogmsg";

  if (argc == 3) {
	  const char *model_path2 = svn_path_canonicalize(argv[2], pool);
	  dbip = db_open(model_path2, "r");
	  if(dbip == DBI_NULL) {
		  printf("could not open %s\n", model_path);
		  exit(EXIT_FAILURE);
	  }
	  (void)db_dirbuild(dbip);
	  wdbp = wdb_dbopen(dbip, RT_WDB_TYPE_DB_DISK);
	  db_update_nref(dbip, &rt_uniresource);

	  /* will need to use an iterpool in here: http://www.opensubscriber.com/message/users@subversion.tigris.org/8428443.html */
	  struct commit_items *update_list;
	  BU_GETSTRUCT(update_list, commit_items);
	  BU_LIST_INIT(&(update_list->l));
	  for (inc=0; inc < RT_DBNHASH; inc++) {
		  for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
			  if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
				  data = bu_malloc(sizeof(struct bu_external), "alloc external data struct");
				  rt_data_stream = svn_stream_empty(pool);
				  rt_db_get_internal5(ip, dp, dbip, NULL, &rt_uniresource);
				  rt_db_cvt_to_external5(data, dp->d_namep, ip, 1, dbip,  &rt_uniresource, ip->idb_major_type);
				  if(check_obj(pool, update_list, repos, youngest_rev, model_name, dp->d_namep, data) != 1) {
					  bu_free(data->ext_buf, "free buff");
					  bu_free(data, "free data");
				  }
			  }
		  }
	  }
	  if (!BU_LIST_IS_EMPTY(&(update_list->l))) commit_objs(pool, update_list, repos, youngest_rev, repo_full_path, model_name, user, logmsg);
	  bu_vls_free(&filedir);
	  bu_vls_free(&filepath);
	  db_close(dbip);
  }

  /* make staging area */
  char *staging_area= "./GS_staging";
  const char *full_staging_area= svn_path_canonicalize(staging_area, pool);
  if (!bu_file_exists(full_staging_area)){
	  if(mkdir(full_staging_area, (S_IRWXU | S_IRWXG | S_IRWXO))) {
		  printf("mkdir failed: %s\n", full_staging_area);
		  exit(EXIT_FAILURE);
	  }
  }

  /* time for change application */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("apply changes: %d sec\n", tdiff);
  t0 = time(NULL);

/* Reassemble .g info */
  struct assemble_info ainfo;
  bu_vls_init(&ainfo.svn_file);
  apr_hash_t *objects = apr_hash_make(pool);
  apr_hash_index_t *obj;
  svn_fs_root_t *repo_root;
  const void *key;
  apr_ssize_t klen;
  svn_string_t *model_file = svn_string_createf(pool, "%s/%s", full_staging_area, model_name);
  svn_revnum_t revnum = 1;
  ainfo.model_name = model_name;
  ainfo.model_file = model_file->data;
  ainfo.pool = pool;
  svn_fs_youngest_rev(&revnum, fs, pool);
  svn_fs_revision_root(&repo_root, fs, revnum, pool);
  ainfo.root = repo_root;
  if (!bu_file_exists(ainfo.model_file)){
	  ainfo.wdbp = wdb_fopen(ainfo.model_file);
	  ainfo.dbip = ainfo.wdbp->dbip;
  } else {
	  ainfo.dbip = db_open(ainfo.model_file, "w");
	  ainfo.wdbp = wdb_dbopen(dbip, RT_WDB_TYPE_DB_DISK_APPEND_ONLY);
  }
  svn_fs_dir_entries(&objects, repo_root, model_name, pool);
  for (obj = apr_hash_first(pool, objects); obj; obj = apr_hash_next(obj)) {
	 apr_hash_this(obj, &key, &klen, NULL);
	 (void)concat_obj((void *)&ainfo, (const void *)key, klen, NULL);
  }
  bu_vls_free(&ainfo.svn_file);
  db_close(ainfo.dbip);

  /* time for reassembly */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("reassemble .g file: %d sec\n", tdiff);
  t0 = time(NULL);

  /* total time */
  tdiff = (int)difftime(t0,tb);
  printf("total delta: %d sec\n", tdiff);

  /* run g_diff */
  bu_vls_init(&vstr);
  bu_vls_sprintf(&vstr, "./GS_staging/%s", model_name);
  /*(void)execlp("g_diff", "g_diff", filename, bu_vls_addr(&gs_user), NULL);*/

  /* Done, now clean up */
  bu_vls_free(&vstr);
  svn_pool_destroy(pool);
}

