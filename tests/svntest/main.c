#include "common.h"

#include <apr_file_io.h>
#include <apr_signal.h>

#include "svn_pools.h"
#include "svn_cmdline.h"
#include "svn_error.h"
#include "svn_opt.h"
#include "svn_utf.h"
#include "svn_subst.h"
#include "svn_path.h"
#include "svn_config.h"
#include "svn_repos.h"
#include "svn_fs.h"
#include "svn_version.h"
#include "svn_props.h"
#include "svn_time.h"
#include "svn_user.h"
#include "svn_client.h"
#include "svn_wc.h"

#include "private/svn_opt_private.h"

#include "svn_private_config.h"

#include <sys/stat.h>

#include "bu.h"
#include "vmath.h"
#include "bn.h"
#include "dg.h"
#include "mater.h"
#include "libtermio.h"
#include "db.h"
#include "ged.h"

struct keep_node_data {
	struct rt_wdb *wdbp;
	struct ged *gedp;
};


/*** Code. ***/

/* A flag to see if we've been cancelled. */
static volatile sig_atomic_t cancelled = FALSE;

/* A signal handler to support cancellation. */
static void
signal_handler(int signum)
{
  apr_signal(signum, SIG_IGN);
  cancelled = TRUE;
}


/* A helper to set up the cancellation signal handlers. */
static void
setup_cancellation_signals(void (*handler)(int signum))
{
  apr_signal(SIGINT, handler);
#ifdef SIGBREAK
  /* SIGBREAK is a Win32 specific signal generated by ctrl-break. */
  apr_signal(SIGBREAK, handler);
#endif
#ifdef SIGHUP
  apr_signal(SIGHUP, handler);
#endif
#ifdef SIGTERM
  apr_signal(SIGTERM, handler);
#endif
}


/* Our cancellation callback. */
static svn_error_t *
check_cancel(void *baton)
{
  if (cancelled)
    return svn_error_create(SVN_ERR_CANCELLED, NULL, _("Caught signal"));
  else
    return SVN_NO_ERROR;
}


/* Helper to open stdio streams */
static svn_error_t *
create_stdio_stream(svn_stream_t **stream,
                    APR_DECLARE(apr_status_t) open_fn(apr_file_t **,
                                                      apr_pool_t *),
                    apr_pool_t *pool)
{
  apr_file_t *stdio_file;
  apr_status_t apr_err = open_fn(&stdio_file, pool);

  if (apr_err)
    return svn_error_wrap_apr(apr_err, _("Can't open stdio file"));

  *stream = svn_stream_from_aprfile2(stdio_file, TRUE, pool);
  return SVN_NO_ERROR;
}


/* Helper to parse local repository path.  Try parsing next parameter
 * of OS as a local path to repository.  If successfull *REPOS_PATH
 * will contain internal style path to the repository.
 */
static svn_error_t *
parse_local_repos_path(apr_getopt_t *os,
                       const char ** repos_path,
                       apr_pool_t *pool)
{
  *repos_path = NULL;

  /* Check to see if there is one more parameter. */
  if (os->ind < os->argc)
    {
      const char * path = os->argv[os->ind++];
      SVN_ERR(svn_utf_cstring_to_utf8(repos_path, path, pool));
      *repos_path = svn_path_internal_style(*repos_path, pool);
    }

  if (*repos_path == NULL)
    {
      return svn_error_create(SVN_ERR_CL_ARG_PARSING_ERROR, NULL,
                              _("Repository argument required"));
    }
  else if (svn_path_is_url(*repos_path))
    {
      return svn_error_createf(SVN_ERR_CL_ARG_PARSING_ERROR, NULL,
                               _("'%s' is an URL when it should be a path"),
                               *repos_path);
    }

  return SVN_NO_ERROR;
}


/* Custom filesystem warning function. */
static void
warning_func(void *baton,
             svn_error_t *err)
{
  if (! err)
    return;
  svn_handle_error2(err, stderr, FALSE, "svntest: ");
}


/* Helper to open a repository and set a warning func (so we don't
 * SEGFAULT when libsvn_fs's default handler gets run).  */
static svn_error_t *
open_repos(svn_repos_t **repos,
           const char *path,
           apr_pool_t *pool)
{
  SVN_ERR(svn_repos_open(repos, path, pool));
  svn_fs_set_warning_func(svn_repos_fs(*repos), warning_func, NULL);
  return SVN_NO_ERROR;
}


/* Version compatibility check */
static svn_error_t *
check_lib_versions(void)
{
  static const svn_version_checklist_t checklist[] =
    {
      { "svn_subr",  svn_subr_version },
      { "svn_repos", svn_repos_version },
      { "svn_fs",    svn_fs_version },
      { "svn_delta", svn_delta_version },
      { NULL, NULL }
    };

  SVN_VERSION_DEFINE(my_version);
  return svn_ver_check_list(&my_version, checklist);
}


/* Baton for passing option/argument state to a subcommand function. */
struct svntest_opt_state
{
  const char *repository_path;
  const char *new_repository_path;                  /* hotcopy dest. path */
  const char *fs_type;                              /* --fs-type */
  svn_boolean_t pre_1_4_compatible;                 /* --pre-1.4-compatible */
  svn_boolean_t pre_1_5_compatible;                 /* --pre-1.5-compatible */
  svn_boolean_t pre_1_6_compatible;                 /* --pre-1.6-compatible */
  svn_opt_revision_t start_revision, end_revision;  /* -r X[:Y] */
  svn_boolean_t help;                               /* --help or -? */
  svn_boolean_t version;                            /* --version */
  svn_boolean_t incremental;                        /* --incremental */
  svn_boolean_t use_deltas;                         /* --deltas */
  svn_boolean_t use_pre_commit_hook;                /* --use-pre-commit-hook */
  svn_boolean_t use_post_commit_hook;               /* --use-post-commit-hook */
  svn_boolean_t use_pre_revprop_change_hook;        /* --use-pre-revprop-change-hook */
  svn_boolean_t use_post_revprop_change_hook;       /* --use-post-revprop-change-hook */
  svn_boolean_t quiet;                              /* --quiet */
  svn_boolean_t bdb_txn_nosync;                     /* --bdb-txn-nosync */
  svn_boolean_t bdb_log_keep;                       /* --bdb-log-keep */
  svn_boolean_t clean_logs;                         /* --clean-logs */
  svn_boolean_t bypass_hooks;                       /* --bypass-hooks */
  svn_boolean_t wait;                               /* --wait */
  enum svn_repos_load_uuid uuid_action;             /* --ignore-uuid,
                                                       --force-uuid */
  const char *parent_dir;

  const char *config_dir;    /* Overriding Configuration Directory */
};


/* This implements `svn_opt_subcommand_t'. */
static svn_error_t *
subcommand_create(void *baton, apr_pool_t *pool)
{
  struct svntest_opt_state *opt_state = baton;
  svn_repos_t *repos;
  apr_hash_t *config;
  apr_hash_t *fs_config = apr_hash_make(pool);

  apr_hash_set(fs_config, SVN_FS_CONFIG_BDB_TXN_NOSYNC,
               APR_HASH_KEY_STRING,
               (opt_state->bdb_txn_nosync ? "1" : "0"));

  apr_hash_set(fs_config, SVN_FS_CONFIG_BDB_LOG_AUTOREMOVE,
               APR_HASH_KEY_STRING,
               (opt_state->bdb_log_keep ? "0" : "1"));

  if (opt_state->fs_type)
    apr_hash_set(fs_config, SVN_FS_CONFIG_FS_TYPE,
                 APR_HASH_KEY_STRING,
                 opt_state->fs_type);

  if (opt_state->pre_1_4_compatible)
    apr_hash_set(fs_config, SVN_FS_CONFIG_PRE_1_4_COMPATIBLE,
                 APR_HASH_KEY_STRING,
                 "1");

  if (opt_state->pre_1_5_compatible)
    apr_hash_set(fs_config, SVN_FS_CONFIG_PRE_1_5_COMPATIBLE,
                 APR_HASH_KEY_STRING,
                 "1");

  if (opt_state->pre_1_6_compatible)
    apr_hash_set(fs_config, SVN_FS_CONFIG_PRE_1_6_COMPATIBLE,
                 APR_HASH_KEY_STRING,
                 "1");

  SVN_ERR(svn_config_get_config(&config, opt_state->config_dir, pool));
  SVN_ERR(svn_repos_create(&repos, opt_state->repository_path,
                           NULL, NULL,
                           config, fs_config, pool));
  svn_fs_set_warning_func(svn_repos_fs(repos), warning_func, NULL);
  return SVN_NO_ERROR;
}




/** Main. **/

int
main(int argc, const char *argv[])
{
  svn_error_t *err;
  apr_status_t apr_err;
  apr_allocator_t *allocator;
  apr_pool_t *pool;

  const svn_opt_subcommand_desc2_t *subcommand = NULL;
  struct svntest_opt_state opt_state;
  int opt_id;
  int i;

  /* Initialize the app. */
  if (svn_cmdline_init("svntest", stderr) != EXIT_SUCCESS)
    return EXIT_FAILURE;

  /* Create our top-level pool.  Use a separate mutexless allocator,
   * given this application is single threaded.
   */
  if (apr_allocator_create(&allocator))
    return EXIT_FAILURE;

  apr_allocator_max_free_set(allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);

  pool = svn_pool_create_ex(NULL, allocator);
  apr_allocator_owner_set(allocator, pool);

  /* Check library versions */
  err = check_lib_versions();
  if (err)
    return svn_cmdline_handle_exit_error(err, pool, "svntest: ");

  /* Initialize the FS library. */
  err = svn_fs_initialize(pool);
  if (err)
    return svn_cmdline_handle_exit_error(err, pool, "svntest: ");

  /* Initialize opt_state. */
  memset(&opt_state, 0, sizeof(opt_state));
  opt_state.start_revision.kind = svn_opt_revision_unspecified;
  opt_state.end_revision.kind = svn_opt_revision_unspecified;

  /* Set up our cancellation support. */
  setup_cancellation_signals(signal_handler);

#ifdef SIGPIPE
  /* Disable SIGPIPE generation for the platforms that have it. */
  apr_signal(SIGPIPE, SIG_IGN);
#endif

#ifdef SIGXFSZ
  /* Disable SIGXFSZ generation for the platforms that have it, otherwise
   * working with large files when compiled against an APR that doesn't have
   * large file support will crash the program, which is uncool. */
  apr_signal(SIGXFSZ, SIG_IGN);
#endif

  char *repo_path = "./test_repository";
  const char *full_path = svn_path_canonicalize(repo_path, pool);
  opt_state.repository_path = full_path;

  /* Check if it's already created - if it is, don't try to re-create it */
  if(svn_repos_find_root_path(full_path, pool)){
    printf("Repository %s already exists, continuing with test.\n", full_path);
  } else {
  /* Run the subcommand. */
  err = subcommand_create(&opt_state, pool);
  if (err)
    {
      /* For argument-related problems, suggest using the 'help'
         subcommand. */
      if (err->apr_err == SVN_ERR_CL_INSUFFICIENT_ARGS
          || err->apr_err == SVN_ERR_CL_ARG_PARSING_ERROR)
        {
          err = svn_error_quick_wrap(err,
                                     _("Try 'svntest help' for more info"));
        }
      return svn_cmdline_handle_exit_error(err, pool, "svntest: ");
    }
  else
    {
      printf("Created repository: %s\n", full_path);
    }
  }

  /* Have repository, do something with it */

  svn_client_ctx_t *ctx;
  if ((err = svn_client_create_context(&ctx, pool)))
	  return svn_cmdline_handle_exit_error(err, pool, "svntest: ");
  err = svn_config_get_config(&(ctx->config),
		  opt_state.config_dir, pool);
  if (err)
  {
	  /* Fallback to default config if the config directory isn't readable. */
	  if (err->apr_err == APR_EACCES)
	  {
		  svn_handle_warning2(stderr, err, "svn: ");
		  svn_error_clear(err);
	  }
	  else
		  return svn_cmdline_handle_exit_error(err, pool, "svn: ");
  }

  /* Next, check out a working copy */

  const char *abs_path;
  char full_repository_url[1024];
  svn_path_get_absolute(&abs_path, repo_path, pool);
  sprintf(full_repository_url,"file://localhost:%s", abs_path);
  printf("full_repository_url: %s\n", full_repository_url);

  char *checkout_path1 = "./test_checkout1";
  char *checkout_path2 = "./test_checkout2";
  const char *full_checkout_path1 = svn_path_canonicalize(checkout_path1, pool);
  const char *full_checkout_path2 = svn_path_canonicalize(checkout_path2, pool);
  svn_opt_revision_t revision;
  svn_opt_revision_t peg_revision;
  revision.kind = svn_opt_revision_head;
  peg_revision.kind = svn_opt_revision_unspecified;

  apr_pool_t *subpool;
  subpool = svn_pool_create(pool);
  svn_pool_clear(subpool);
  printf("svn checkout:  repo: %s, target: %s\n", full_repository_url, full_checkout_path1);
  svn_client_checkout3(NULL, full_repository_url, full_checkout_path1, &peg_revision, &revision, svn_depth_infinity, 0, 0, ctx, subpool);

  svn_pool_clear(subpool);
  printf("svn checkout:  repo: %s, target: %s\n", full_repository_url, full_checkout_path2);
  svn_client_checkout3(NULL, full_repository_url, full_checkout_path2, &peg_revision, &revision, svn_depth_infinity, 0, 0, ctx, subpool);
 
  struct db_i *dbip = DBI_NULL;
  struct db_i *new_dbip = DBI_NULL;
  struct rt_wdb *wdbp = RT_WDB_NULL;
  struct rt_wdb *keepfp = RT_WDB_NULL;
  struct ged gedp;
  dbip = db_open("./havoc.g", "r");
  if(dbip == DBI_NULL) {
     printf("need ./havoc.g\n");
     exit(EXIT_FAILURE);
  }

  /* list of targets to commit */
  apr_array_header_t *targets = apr_array_make(pool, 5, sizeof(const char *));
  
  /* Add a new directory to the first working copy and svn add it to the repository */
  char parent_path[1024];
  sprintf(parent_path,"havoc.g", full_checkout_path1);
  char file_path[1024];
  sprintf(file_path,"%s/%s", full_checkout_path1, parent_path);
  printf("file_path: %s\n", file_path);
  if(mkdir(file_path, (S_IRWXU | S_IRWXG | S_IRWXO))) {
     printf("mkdir failed: %s\n", file_path);
     exit(EXIT_FAILURE);
  } else { 
     svn_pool_clear(subpool);
     svn_client_add4(file_path, svn_depth_empty, FALSE, FALSE, FALSE, ctx, subpool);
     *(const char**)apr_array_push(targets) = apr_pstrdup(targets->pool, file_path);
  }

  char pieces_path[1024];
  sprintf(pieces_path,"%s/havoc_contents", parent_path);
  sprintf(file_path,"%s/%s", full_checkout_path1, pieces_path);
  printf("file_path: %s\n", file_path);
  if(mkdir(file_path, (S_IRWXU | S_IRWXG | S_IRWXO))) {
     printf("mkdir failed: %s\n", file_path);
     exit(EXIT_FAILURE);
  } else { 
     svn_pool_clear(subpool);
     svn_client_add4(file_path, svn_depth_empty, FALSE, FALSE, FALSE, ctx, subpool);
     *(const char**)apr_array_push(targets) = apr_pstrdup(targets->pool, file_path);
  }

#if 0
  FILE *fp;
  fp = fopen(file_path,"w");
  if(fp != NULL){
     fputs ("subversion test", fp);
     fclose (fp);
  } else {
     printf("augh - no file written: %s\n", file_path);
  }
#endif

#if 0
  /* Beginnings of code intended to use search results to identify
   * regions and assemblies.
   */
  struct rt_search_dbinfo dbinfo;
  struct rt_search_results results;
  struct rt_search_dir_list *entry;
  dbinfo.dbip = dbip;
  dbinfo.wdbp = wdbp;
  bu_vls_init(&results.result_str);
  BU_LIST_INIT(&(results.dir_list.l));
  /* will have to construct an argc/argv pair with the assembly search
   * logic to feed to rtsearch
   */
  rt_search(&dbinfo, &results, argc, argv);
  while (BU_LIST_WHILE(entry, rt_search_dir_list, &(results.dir_list.l))) {
	  bu_log("Entry value is %s\n", entry->dp->d_namep);
	  BU_LIST_DEQUEUE(&(entry->l));
	  bu_free(entry, "free rt_search_dir_list entry");
  }
#endif


  svn_pool_clear(subpool);
  svn_client_add4(file_path, svn_depth_empty, FALSE, FALSE, FALSE, ctx, subpool);
  *(const char**)apr_array_push(targets) = apr_pstrdup(targets->pool, file_path);

  (void)db_dirbuild(dbip);
  wdbp = wdb_dbopen(dbip, RT_WDB_TYPE_DB_DISK);
  GED_INIT(&gedp, wdbp);
  ged_tops(&gedp, 1, NULL);
  printf("tops: %s\n", bu_vls_addr(&gedp.ged_result_str));
  db_update_nref(dbip, &rt_uniresource);
  int inc;
  struct directory *dp;
  struct keep_node_data knd;
  for(inc = 0; inc < RT_DBNHASH; inc++) {
	  for (dp = dbip->dbi_Head[inc]; dp != RT_DIR_NULL; dp = dp->d_forw) {
		  if (dp->d_nref != 0) {
			  sprintf(file_path, "%s/%s/%s", full_checkout_path1, pieces_path, dp->d_namep);
			  printf("file_path: %s\n", file_path);
			  keepfp = wdb_fopen_v(file_path, db_version(dbip));
			  knd.wdbp = keepfp;
			  knd.gedp = &gedp;
			  db_update_ident(keepfp->dbip, "svn test", dbip->dbi_local2base);
			  node_write(dbip, dp, (genptr_t)&knd);
			  wdb_close(keepfp);
			  svn_pool_clear(subpool);
			  svn_client_add4(file_path, svn_depth_empty, FALSE, FALSE, FALSE, ctx, subpool);
  			  *(const char**)apr_array_push(targets) = apr_pstrdup(targets->pool, file_path);
		  } else {
			  if(!BU_STR_EQUAL(dp->d_namep, "_GLOBAL")) {
			  sprintf(file_path, "%s/%s/%s", full_checkout_path1, parent_path, dp->d_namep);
			  printf("toplevel: %s\n", file_path);
			  keepfp = wdb_fopen_v(file_path, db_version(dbip));
			  knd.wdbp = keepfp;
			  knd.gedp = &gedp;
			  db_update_ident(keepfp->dbip, "svn test", dbip->dbi_local2base);
			  node_write(dbip, dp, (genptr_t)&knd);
			  wdb_close(keepfp);
			  svn_pool_clear(subpool);
			  svn_client_add4(file_path, svn_depth_empty, FALSE, FALSE, FALSE, ctx, subpool);
  			  *(const char**)apr_array_push(targets) = apr_pstrdup(targets->pool, file_path);
			  }
		  }
	  }
  }

  for(i=0; i< targets->nelts;  i++){
	  const char *s = ((const char**)targets->elts)[i];
	  printf("%d: %s\n", i, s);
  }

  /* Commit the changes */
  svn_pool_clear(subpool);
  svn_commit_info_t *commit_info = NULL;
  svn_client_commit4(&commit_info, targets, svn_depth_empty, FALSE, FALSE, NULL, NULL, ctx, subpool);

  printf("committed the changes\n");
  
  /* Perform an update operation on the second repository */
  svn_pool_clear(subpool);
  apr_array_header_t *update_targets = apr_array_make(pool, 5, sizeof(const char *));
  APR_ARRAY_PUSH(update_targets, const char *) = full_checkout_path2;
  svn_opt_revision_t svnrev;
  svnrev.kind = svn_opt_revision_unspecified;
  svn_client_update3(NULL, update_targets, &svnrev, svn_depth_unknown, 0, 0, 0, ctx, subpool);

  printf("updated second repository\n");
  
  /* Done, now clean up */
  svn_pool_destroy(pool);
  /* Ensure that everything is written to stdout, so the user will
     see any print errors. */
  err = svn_cmdline_fflush(stdout);
  if (err)
  {
	  svn_handle_error2(err, stderr, FALSE, "svntest: ");
	  svn_error_clear(err);
	  return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;

}

