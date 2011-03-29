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

  svn_filesize_t buflen;
  svn_stream_t *obj_contents = svn_stream_empty(subpool);
  svn_stringbuf_t *stringbuf;
  struct bu_external data;
  BU_INIT_EXTERNAL(&data);
  RT_INIT_DB_INTERNAL(&ip);

  bu_vls_sprintf(&ainfo->svn_file, "%s/%s/%s", ainfo->model_name, (const char *)objname, (const char *)objname);
  /* get svn_file contents and convert them into the right form */
  svn_fs_file_length(&buflen, ainfo->root, bu_vls_addr(&ainfo->svn_file), subpool);
  data.ext_nbytes = (size_t)buflen;
  data.ext_buf = bu_malloc(data.ext_nbytes, "memory for .g data");
  svn_fs_file_contents(&obj_contents, ainfo->root, bu_vls_addr(&ainfo->svn_file), subpool);
  svn_stream_read(obj_contents, (char *)data.ext_buf, (apr_size_t *)&buflen);
  svn_stream_close(obj_contents);
 
  /* Put things into the new database */ 
  rt_db_external5_to_internal5(&ip, &data, (const char *)objname, ainfo->dbip, NULL, &rt_uniresource);
  wdb_put_internal(ainfo->wdbp, (const char *)objname, &ip, 1);
  
  svn_pool_destroy(subpool);
  bu_free(data.ext_buf, "free ext buf");
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
  const char *full_path = svn_path_canonicalize(repo_path, pool);

  /* Check if it's already created - if it is, don't try to re-create it */
  if(svn_repos_find_root_path(full_path, pool)){
    printf("Repository %s already exists, continuing with test.\n", full_path);
  } else {
      svn_repos_create(&repos, full_path, NULL, NULL, NULL, NULL, pool);
      printf("Created repository: %s\n", full_path);
  }

  svn_repos_open(&repos, full_path, pool);
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


   /* make staging area */
  char *staging_area= "./GS_staging";
  const char *full_staging_area= svn_path_canonicalize(staging_area, pool);
  if (!bu_file_exists(full_staging_area)){
	  if(mkdir(full_staging_area, (S_IRWXU | S_IRWXG | S_IRWXO))) {
		  printf("mkdir failed: %s\n", full_staging_area);
		  exit(EXIT_FAILURE);
	  }
  }

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

  svn_fs_youngest_rev(&youngest_rev, fs, pool);
  svn_repos_fs_begin_txn_for_commit2(&txn, repos, youngest_rev, apr_hash_make(pool), pool);
  svn_fs_txn_root(&txn_root, txn, pool);
  bu_vls_sprintf(&filepath, "%s/%s", model_name, "TESTFILE");
  svn_fs_make_file (txn_root, bu_vls_addr(&filepath), pool);
  char *testcontents = "test1";
  apr_size_t length = strlen(testcontents);
  svn_fs_apply_text (&rt_data_stream, txn_root, bu_vls_addr(&filepath), NULL, pool);
  svn_stream_write(rt_data_stream, (const char *)testcontents, &length);
  svn_stream_close(rt_data_stream);
  svn_repos_fs_commit_txn(NULL, repos, &youngest_rev, txn, pool);

  const svn_delta_editor_t *editor;
  void *edit_baton;
  char *user = "testuser";
  char *logmsg = "testlogmsg";

  svn_fs_youngest_rev(&youngest_rev, fs, pool);
  svn_repos_fs_begin_txn_for_commit2(&txn, repos, youngest_rev, apr_hash_make(pool), pool);
  svn_repos_get_commit_editor4(&editor, &edit_baton, repos, txn, full_path, NULL, user, logmsg, NULL, NULL, NULL, NULL, pool);
  svn_fs_txn_root(&txn_root, txn, pool);
  bu_vls_sprintf(&filepath, "%s/%s", model_name, "TESTFILE");
  char *testcontents2 = "test2";
  svn_fs_apply_text (&rt_data_stream, txn_root, bu_vls_addr(&filepath), NULL, pool);
  svn_stream_write(rt_data_stream, (const char *)testcontents2, &length);
  svn_stream_close(rt_data_stream);
  editor->close_edit;



 
  /* run g_diff */
  bu_vls_init(&vstr);
  bu_vls_sprintf(&vstr, "./GS_staging/%s", model_name);
  /*(void)execlp("g_diff", "g_diff", filename, bu_vls_addr(&gs_user), NULL);*/

  /* Done, now clean up */
  bu_vls_free(&vstr);
  svn_pool_destroy(pool);
}

