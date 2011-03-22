
#ifndef VCS_EXPORT
#  if defined(_WIN32) && !defined(__CYGWIN__) && defined(BRLCAD_DLL)
#    ifdef VCS_EXPORT_DLL
#      define VCS_EXPORT __declspec(dllexport)
#    else
#      define VCS_EXPORT __declspec(dllimport)
#    endif
#  else
#    define VCS_EXPORT
#  endif
#endif

#ifndef VCS_EXTERN
#  define VCS_EXTERN(type_and_name, args) extern type_and_name args
#endif


/**
 * Parent strucutre for a versioned geometry database
 */

struct vcs_db {
   char *zRepositoryName;  	/* Name of the repository database */
   const char *zMainDbType;	/* "configdb", "localdb", or "repository" */
   sqlite3 *db;			/* Connection to the main database */
   sqlite3 *dbConfig;		/* Connection to the config database */
   /* status variables */
   int configAttach;		/* True if global_config is attached to repository */
   int configOpen;         	/* True if the config database is open */
   int repositoryOpen;     	/* True if the main repository database is open */
   int localOpen;          	/* True if the local database is open */
   /* user information */
   const char *zLogin;     	/* Login name.  "" if not logged in. */
   int useLocalauth;       	/* No login required if from 127.0.0.1 */
   int noPswd;             	/* Logged in without password (on 127.0.0.1) */
   int userUid;            	/* Integer user id */
}


/* Convenience definitions for VCS initialization 
 * return codes */
#define VCS_INIT_SUCCESS 0
#define VCS_INIT_FILE_EXISTS_FAIL 1
#define VCS_INIT_SQL_INIT_FAIL 2
#define VCS_INIT_OPEN_FAIL 3

/**
 * Initialize a new geometry service sql database 
 *
 * Creates a new file and performs basic initialization.
 */
VCS_EXPORT VCS_EXTERN(int geomvcs_init, (char *reponame, char *adminuser, char *initpasswd));
