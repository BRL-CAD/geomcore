#include "common.h"

#include <time.h>
#include <sys/stat.h>

#include <apr_file_io.h>
#include <apr_signal.h>
#include <apr_tables.h>

#include "svn_pools.h"
#include "svn_path.h"
#include "svn_repos.h"
#include "svn_fs.h"

#include "bu.h"
#include "vmath.h"
#include "bn.h"
#include "dg.h"
#include "mater.h"
#include "libtermio.h"
#include "db.h"
#include "raytrace.h"
#include "ged.h"

struct keep_node_data {
	struct rt_wdb *wdbp;
};

void
node_write(struct db_i *dbip, struct directory *dp, genptr_t ptr)
{
	struct keep_node_data *kndp = (struct keep_node_data *)ptr;
	struct rt_db_internal intern;

	RT_CK_WDB(kndp->wdbp);
/*
	if (dp->d_nref++ > 0)
		return;      
*/
	if (rt_db_get_internal(&intern, dp, dbip, NULL, &rt_uniresource) < 0) {
		return;
	}

	/* if this is an extrusion, keep the referenced sketch */
	if (dp->d_major_type == DB5_MAJORTYPE_BRLCAD && dp->d_minor_type == DB5_MINORTYPE_BRLCAD_EXTRUDE) {
		struct rt_extrude_internal *extr;
		struct directory *dp2;

		extr = (struct rt_extrude_internal *)intern.idb_ptr;
		RT_EXTRUDE_CK_MAGIC(extr);

		if ((dp2 = db_lookup(dbip, extr->sketch_name, LOOKUP_QUIET)) != RT_DIR_NULL) {
			node_write(dbip, dp2, ptr);
		}
	} else if (dp->d_major_type == DB5_MAJORTYPE_BRLCAD && dp->d_minor_type == DB5_MINORTYPE_BRLCAD_DSP) {
		struct rt_dsp_internal *dsp;
		struct directory *dp2;

		/* this is a DSP, if it uses a binary object, keep it also */
		dsp = (struct rt_dsp_internal *)intern.idb_ptr;
		RT_DSP_CK_MAGIC(dsp);

		if (dsp->dsp_datasrc == RT_DSP_SRC_OBJ) {
			/* need to keep this object */
			if ((dp2 = db_lookup(dbip, bu_vls_addr(&dsp->dsp_name),  LOOKUP_QUIET)) != RT_DIR_NULL) {
				node_write(dbip, dp2, ptr);
			}
		}
	}

	if (wdb_put_internal(kndp->wdbp, dp->d_namep, &intern, 1.0) < 0) {
		return;
	}
}

/* This implements the svn_client_list_func_t API, printing a single
   directory entry in text format. */
int concat_obj(const char *path, const char *target, apr_pool_t *pool)
{
  const char *entryname, *modelpath;
  struct db_i *input_dbip;
  struct db_i *collecting_dbip;
  struct directory *dp, *new_dp;
  struct rt_db_internal ip;
  struct bu_vls model,entry;

  bu_vls_init(&model);
  bu_vls_init(&entry);
  entryname = svn_path_basename(path, pool);
  modelpath = svn_path_dirname(path, pool);
  target = svn_path_dirname(modelpath, pool);
  bu_vls_sprintf(&model, "GS_staging/%s", target);
  if (!bu_file_exists(bu_vls_addr(&model))){
	  collecting_dbip = db_create(bu_vls_addr(&model), 5);
  } else {
	  collecting_dbip = db_open(bu_vls_addr(&model), "w");
  }
  bu_vls_sprintf(&entry, "GS_%s/%s", target, path);
  /*printf("Adding %s to %s\n", bu_vls_addr(&entry), bu_vls_addr(&model));*/
  input_dbip = db_open(bu_vls_addr(&entry), "r");
  (void)db_dirbuild(input_dbip);
  db_update_nref(input_dbip, &rt_uniresource);
  dp = db_lookup(input_dbip, entryname, LOOKUP_QUIET);
  rt_db_get_internal( &ip, dp, input_dbip, NULL, &rt_uniresource);
  new_dp = db_diradd( collecting_dbip, dp->d_namep, RT_DIR_PHONY_ADDR, 0, dp->d_flags, (genptr_t)&dp->d_minor_type );
  rt_db_put_internal( new_dp, collecting_dbip, &ip, &rt_uniresource );

  db_close(collecting_dbip);
  db_close(input_dbip);
  bu_vls_free(&entry); 
  bu_vls_free(&model); 
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
  const char **conflict_p;
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
  struct bu_external *data;
  struct bu_vls filedir;
  struct bu_vls filepath;
  struct svn_string_t *rt_contents;
  struct svn_stream_t *rt_data_stream;
  bu_vls_init(&filedir);
  bu_vls_init(&filepath);

  /* will need to use an iterpool in here: http://www.opensubscriber.com/message/users@subversion.tigris.org/8428443.html */
  for (inc=0; inc < RT_DBNHASH; inc++) {
	  for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
		  if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
			  db_get_external(data, dp, dbip);
			  bu_vls_sprintf(&filedir, "%s/%s", model_name, dp->d_namep);
			  bu_vls_sprintf(&filepath, "%s/%s/%s", model_name, dp->d_namep, dp->d_namep);
			  svn_fs_make_dir(txn_root, bu_vls_addr(&filedir), pool);
			  svn_fs_make_file (txn_root, bu_vls_addr(&filepath), pool);
			  rt_contents = svn_string_ncreate((const char *)data->ext_buf, (apr_size_t)data->ext_nbytes, pool);
			  rt_data_stream = svn_stream_from_string(rt_contents, pool);
			  svn_fs_apply_text (&rt_data_stream, txn_root, bu_vls_addr(&filepath), NULL, pool);
			  svn_stream_close(rt_data_stream);
		  }
	  }
  }
  bu_vls_free(&filedir);
  bu_vls_free(&filepath);

  /* time for breakout and insertion*/
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf(".g breakout and insertion: %d sec\n", tdiff);
  t0 = time(NULL);


  /* Commit the changes */
  svn_repos_fs_commit_txn(conflict_p, repos, &youngest_rev, txn, pool);

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

  /* Re-assemble .g files */
  /*look into svn_fs_dir_entries and svn_fs_file_contents here*/

  

  /* time for reassembly */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("reassemble .g file: %d sec\n", tdiff);
  t0 = time(NULL);

  /* time for reassembly */
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

