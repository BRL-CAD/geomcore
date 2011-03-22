
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


/*
** Number of elements in an array
*/
#define count(X)  (sizeof(X)/sizeof(X[0]))

/*
** Size of a UUID in characters
*/
#define UUID_SIZE 40

/*
** Maximum number of auxiliary parameters on reports
*/
#define MX_AUX  5

/**
 * Parent strucutre for a versioned geometry database
 */

struct vcs_db {
   char *zRepositoryName;  	/* Name of the repository database */
   const char *zMainDbType;	/* "configdb", "localdb", or "repository" */
   sqlite3 *db;			/* Connection to the main database */
   sqlite3 *dbConfig;		/* Connection to the config database */
   char *zLocalRoot;       	/* The directory holding the  local database */
   /* status variables */
   int useAttach;		/* True if global_config is attached to repository */
   int configOpen;         	/* True if the config database is open */
   int repositoryOpen;     	/* True if the main repository database is open */
   int localOpen;          	/* True if the local database is open */
   int *aCommitFile;            /* Array of files to be committed */
   /* user information */
   const char *zLogin;     	/* Login name.  "" if not logged in. */
   int useLocalauth;       	/* No login required if from 127.0.0.1 */
   int noPswd;             	/* Logged in without password (on 127.0.0.1) */
   int userUid;            	/* Integer user id */
   const char *zHome;      	/* Name of user home directory */

   /* database error message */
   char *zErrMsg;          	/* Text of an error message */
   int iErrPriority;       	/* Priority of current error message */

   int fSqlTrace;          /* True if --sqltrace flag is present */
   int fSqlStats;          /* True if --sqltrace or --sqlstats are present */
   int fSqlPrint;          /* True if -sqlprint flag is present */

  /* Information used to populate the RCVFROM table */
  int rcvid;              /* The rcvid.  0 if not yet defined. */
  char *zIpAddr;          /* The remote IP address */
  char *zNonce;           /* The nonce used for login */

  int markPrivate;        /* All new artifacts are private if true */

   int xlinkClusterOnly;   /* Set when cloning.  Only process clusters */
   int okRdAddr;           /* e: read email addresses or other private data */
   int argc;
   char **argv;
   char *zTop;
   int mainInFatalError;
int clockSkewSeen;      /* True if clocks on client and server out of sync */
};

/*
 * ** Macro for debugging:
 * */
#define CGIDEBUG(X)  if( g.fDebug ) cgi_debug X


#define blob_size(X)  ((X)->nUsed)
#define blob_buffer(X)  ((X)->aData)
#define count(X)  (sizeof(X)/sizeof(X[0]))
