/*
** Copyright (c) 2006 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License".)

** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
*******************************************************************************
**
** Code for interfacing to the various databases.
**
** There are three separate database files that fossil interacts
** with:
**
**    (1)  The "user" database in ~/.fossil
**
**    (2)  The "repository" database
**
**    (3)  A local checkout database named "_FOSSIL_" or ".fos"
**         and located at the root of the local copy of the source tree.
**
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/basic.h"
#include "geomvcs/blob.h"
#include "geomvcs/printf.h"
#include "geomvcs/schema.h"
#include "geomvcs/sha1.h"
#include "geomvcs/md5.h"
#include "geomvcs/encode.h"
#include "geomvcs/file.h"
#if ! defined(_WIN32)
#  include <pwd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "geomvcs/db.h"


/* Forward declarations for db.c */
const char *cgi_parameter(const char *zName,const char *zDefault);
#define P(x) cgi_parameter((x),0)

/*
** Call this routine when a database error occurs.
*/
static void db_err(struct vcs_db *db, const char *zFormat, ...){
  va_list ap;
  char *z;
  static const char zRebuildMsg[] = 
      "If you have recently updated your fossil executable, you might\n"
      "need to run \"fossil all rebuild\" to bring the repository\n"
      "schemas up to date.\n";
  va_start(ap, zFormat);
  z = vmprintf(db, zFormat, ap);
  va_end(ap);
  fprintf(stderr, "%s\n\n%s", z, zRebuildMsg);
  db_force_rollback(db);
  geomvcs_exit(db, 1);
}

static int nBegin = 0;      /* Nesting depth of BEGIN */
static int isNewRepo = 0;   /* True if the repository is newly created */
static int doRollback = 0;  /* True to force a rollback */
static int nCommitHook = 0; /* Number of commit hooks */
static struct sCommitHook {
  int (*xHook)(void);  /* Functions to call at db_end_transaction() */
  int sequence;        /* Call functions in sequence order */
} aHook[5];
static Stmt *pAllStmt = 0;  /* List of all unfinalized statements */
static int nPrepare = 0;    /* Number of calls to sqlite3_prepare() */

/*
** This routine is called by the SQLite commit-hook mechanism
** just prior to each commit.  All this routine does is verify
** that nBegin really is zero.  That insures that transactions
** cannot commit by any means other than by calling db_end_transaction()
** below.
**
** This is just a safety and sanity check.
*/
static int db_verify_at_commit(void *db){
  if( nBegin ){
    geomvcs_panic((struct vcs_db *)db, "illegal commit attempt");
    return 1;
  }
  return 0;
}

/*
** Begin and end a nested transaction
*/
void db_begin_transaction(struct vcs_db *db){
  if( nBegin==0 ){
    db_multi_exec(db, "BEGIN");
    sqlite3_commit_hook(db->db, db_verify_at_commit, db);
  }
  nBegin++;
}
void db_end_transaction(struct vcs_db *db, int rollbackFlag){
  if( db->db==0 ) return;
  if( nBegin<=0 ) return;
  if( rollbackFlag ) doRollback = 1;
  nBegin--;
  if( nBegin==0 ){
    int i;
    if( doRollback==0 ) leaf_do_pending_checks();
    for(i=0; doRollback==0 && i<nCommitHook; i++){
      doRollback |= aHook[i].xHook();
    }
    while( pAllStmt ){
      db_finalize(db, pAllStmt);
    }
    db_multi_exec(db, doRollback ? "ROLLBACK" : "COMMIT");
    doRollback = 0;
  }
}

/*
** Force a rollback and shutdown the database
*/
void db_force_rollback(struct vcs_db *db){
  static int busy = 0;
  if( busy || db->db==0 ) return;
  busy = 1;
  undo_rollback();
  while( pAllStmt ){
    db_finalize(db, pAllStmt);
  }
  if( nBegin ){
    sqlite3_exec(db->db, "ROLLBACK", 0, 0, 0);
    nBegin = 0;
    if( isNewRepo ){
      db_close(db, 0);
      unlink(db->zRepositoryName);
    }
  }
  busy = 0;
  db_close(db, 0);
}

/*
** Install a commit hook.  Hooks are installed in sequence order.
** It is an error to install the same commit hook more than once.
**
** Each commit hook is called (in order of accending sequence) at
** each commit operation.  If any commit hook returns non-zero,
** the subsequence commit hooks are omitted and the transaction
** rolls back rather than commit.  It is the responsibility of the
** hooks themselves to issue any error messages.
*/
void db_commit_hook(int (*x)(void), int sequence){
  int i;
  assert( nCommitHook < sizeof(aHook)/sizeof(aHook[1]) );
  for(i=0; i<nCommitHook; i++){
    assert( x!=aHook[i].xHook );
    if( aHook[i].sequence>sequence ){
      int s = sequence;
      int (*xS)(void) = x;
      sequence = aHook[i].sequence;
      x = aHook[i].xHook;
      aHook[i].sequence = s;
      aHook[i].xHook = xS;
    }
  }
  aHook[nCommitHook].sequence = sequence;
  aHook[nCommitHook].xHook = x;
  nCommitHook++;
}

/*
** Prepare a Stmt.  Assume that the Stmt is previously uninitialized.
** If the input string contains multiple SQL statements, only the first
** one is processed.  All statements beyond the first are silently ignored.
*/
int db_vprepare(struct vcs_db *db, Stmt *pStmt, int errOk, const char *zFormat, va_list ap){
  int rc;
  char *zSql;
  blob_zero(&pStmt->sql);
  blob_vappendf(db, &pStmt->sql, zFormat, ap);
  va_end(ap);
  zSql = blob_str(db, &pStmt->sql);
  nPrepare++;
  rc = sqlite3_prepare_v2(db->db, zSql, -1, &pStmt->pStmt, 0);
  if( rc!=0 && !errOk ){
    db_err(db, "%s\n%s", sqlite3_errmsg(db->db), zSql);
  }
  pStmt->pNext = pStmt->pPrev = 0;
  pStmt->nStep = 0;
  return rc;
}
int db_prepare(struct vcs_db *db, Stmt *pStmt, const char *zFormat, ...){
  int rc;
  va_list ap;
  va_start(ap, zFormat);
  rc = db_vprepare(db, pStmt, 0, zFormat, ap);
  va_end(ap);
  return rc;
}
int db_prepare_ignore_error(struct vcs_db *db, Stmt *pStmt, const char *zFormat, ...){
  int rc;
  va_list ap;
  va_start(ap, zFormat);
  rc = db_vprepare(db, pStmt, 1, zFormat, ap);
  va_end(ap);
  return rc;
}
int db_static_prepare(struct vcs_db *db, Stmt *pStmt, const char *zFormat, ...){
  int rc = SQLITE_OK;
  if( blob_size(&pStmt->sql)==0 ){
    va_list ap;
    va_start(ap, zFormat);
    rc = db_vprepare(db, pStmt, 0, zFormat, ap);
    pStmt->pNext = pAllStmt;
    pStmt->pPrev = 0;
    if( pAllStmt ) pAllStmt->pPrev = pStmt;
    pAllStmt = pStmt;
    va_end(ap);
  }
  return rc;
}

/*
** Return the index of a bind parameter
*/
static int paramIdx(struct vcs_db *db, Stmt *pStmt, const char *zParamName){
  int i = sqlite3_bind_parameter_index(pStmt->pStmt, zParamName);
  if( i==0 ){
    db_err(db, "no such bind parameter: %s\nSQL: %b", zParamName, &pStmt->sql);
  }
  return i;
}
/*
** Bind an integer, string, or Blob value to a named parameter.
*/
int db_bind_int(struct vcs_db *db, Stmt *pStmt, const char *zParamName, int iValue){
  return sqlite3_bind_int(pStmt->pStmt, paramIdx(db, pStmt, zParamName), iValue);
}
int db_bind_int64(struct vcs_db *db, Stmt *pStmt, const char *zParamName, i64 iValue){
  return sqlite3_bind_int64(pStmt->pStmt, paramIdx(db, pStmt, zParamName), iValue);
}
int db_bind_double(struct vcs_db *db, Stmt *pStmt, const char *zParamName, double rValue){
  return sqlite3_bind_double(pStmt->pStmt, paramIdx(db, pStmt, zParamName), rValue);
}
int db_bind_text(struct vcs_db *db, Stmt *pStmt, const char *zParamName, const char *zValue){
  return sqlite3_bind_text(pStmt->pStmt, paramIdx(db, pStmt, zParamName), zValue,
                           -1, SQLITE_STATIC);
}
int db_bind_null(struct vcs_db *db, Stmt *pStmt, const char *zParamName){
  return sqlite3_bind_null(pStmt->pStmt, paramIdx(db, pStmt, zParamName));
}
int db_bind_blob(struct vcs_db *db, Stmt *pStmt, const char *zParamName, Blob *pBlob){
  return sqlite3_bind_blob(pStmt->pStmt, paramIdx(db, pStmt, zParamName),
                          blob_buffer(pBlob), blob_size(pBlob), SQLITE_STATIC);
}

/* bind_str() treats a Blob object like a TEXT string and binds it
** to the SQL variable.  Constrast this to bind_blob() which treats
** the Blob object like an SQL BLOB.
*/
int db_bind_str(struct vcs_db *db, Stmt *pStmt, const char *zParamName, Blob *pBlob){
  return sqlite3_bind_text(pStmt->pStmt, paramIdx(db, pStmt, zParamName),
                          blob_buffer(pBlob), blob_size(pBlob), SQLITE_STATIC);
}

/*
** Step the SQL statement.  Return either SQLITE_ROW or an error code
** or SQLITE_OK if the statement finishes successfully.
*/
int db_step(Stmt *pStmt){
  int rc;
  rc = sqlite3_step(pStmt->pStmt);
  pStmt->nStep++;
  return rc;
}

/*
** Print warnings if a query is inefficient.
*/
static void db_stats(struct vcs_db *db, Stmt *pStmt){
#ifdef FOSSIL_DEBUG
  int c1, c2, c3;
  const char *zSql = sqlite3_sql(pStmt->pStmt);
  if( zSql==0 ) return;
  c1 = sqlite3_stmt_status(pStmt->pStmt, SQLITE_STMTSTATUS_FULLSCAN_STEP, 1);
  c2 = sqlite3_stmt_status(pStmt->pStmt, SQLITE_STMTSTATUS_AUTOINDEX, 1);
  c3 = sqlite3_stmt_status(pStmt->pStmt, SQLITE_STMTSTATUS_SORT, 1);
  if( c1>pStmt->nStep*4 && strstr(zSql,"/*scan*/")==0 ){
    geomvcs_warning(db, "%d scan steps for %d rows in [%s]", c1, pStmt->nStep, zSql);
  }else if( c2 ){
    geomvcs_warning(db, "%d automatic index rows in [%s]", c2, zSql);
  }else if( c3 && strstr(zSql,"/*sort*/")==0 && strstr(zSql,"/*scan*/")==0 ){
    geomvcs_warning(db, "sort w/o index in [%s]", zSql);
  }
  pStmt->nStep = 0;
#endif
}

/*
** Reset or finalize a statement.
*/
int db_reset(struct vcs_db *db, Stmt *pStmt){
  int rc;
  db_stats(db, pStmt);
  rc = sqlite3_reset(pStmt->pStmt);
  db_check_result(db, rc);
  return rc;
}
int db_finalize(struct vcs_db *db, Stmt *pStmt){
  int rc;
  db_stats(db, pStmt);
  blob_reset(db, &pStmt->sql);
  rc = sqlite3_finalize(pStmt->pStmt);
  db_check_result(db, rc);
  pStmt->pStmt = 0;
  if( pStmt->pNext ){
    pStmt->pNext->pPrev = pStmt->pPrev;
  }
  if( pStmt->pPrev ){
    pStmt->pPrev->pNext = pStmt->pNext;
  }else if( pAllStmt==pStmt ){
    pAllStmt = pStmt->pNext;
  }
  pStmt->pNext = 0;
  pStmt->pPrev = 0;
  return rc;
}

/*
** Return the rowid of the most recent insert
*/
i64 db_last_insert_rowid(struct vcs_db *db){
  return sqlite3_last_insert_rowid(db->db);
}

/*
** Return the number of rows that were changed by the most recent
** INSERT, UPDATE, or DELETE.  Auxiliary changes caused by triggers
** or other side effects are not counted.
*/
int db_changes(struct vcs_db *db){
  return sqlite3_changes(db->db);
}

/*
** Extract text, integer, or blob values from the N-th column of the
** current row.
*/
int db_column_bytes(Stmt *pStmt, int N){
  return sqlite3_column_bytes(pStmt->pStmt, N);
}
int db_column_int(Stmt *pStmt, int N){
  return sqlite3_column_int(pStmt->pStmt, N);
}
i64 db_column_int64(Stmt *pStmt, int N){
  return sqlite3_column_int64(pStmt->pStmt, N);
}
double db_column_double(Stmt *pStmt, int N){
  return sqlite3_column_double(pStmt->pStmt, N);
}
const char *db_column_text(Stmt *pStmt, int N){
  return (char*)sqlite3_column_text(pStmt->pStmt, N);
}
const char *db_column_raw(Stmt *pStmt, int N){
  return (const char*)sqlite3_column_blob(pStmt->pStmt, N);
}
const char *db_column_name(Stmt *pStmt, int N){
  return (char*)sqlite3_column_name(pStmt->pStmt, N);
}
int db_column_count(Stmt *pStmt){
  return sqlite3_column_count(pStmt->pStmt);
}
char *db_column_malloc(struct vcs_db *db, Stmt *pStmt, int N){
  return mprintf(db, "%s", db_column_text(pStmt, N));
}
void db_column_blob(struct vcs_db *db, Stmt *pStmt, int N, Blob *pBlob){
  blob_append(db, pBlob, sqlite3_column_blob(pStmt->pStmt, N),
              sqlite3_column_bytes(pStmt->pStmt, N));
}

/*
** Initialize a blob to an ephermeral copy of the content of a
** column in the current row.  The data in the blob will become
** invalid when the statement is stepped or reset.
*/
void db_ephemeral_blob(Stmt *pStmt, int N, Blob *pBlob){
  blob_init(pBlob, sqlite3_column_blob(pStmt->pStmt, N),
              sqlite3_column_bytes(pStmt->pStmt, N));
}

/*
** Check a result code.  If it is not SQLITE_OK, print the
** corresponding error message and exit.
*/
void db_check_result(struct vcs_db *db, int rc){
  if( rc!=SQLITE_OK ){
    db_err(db, "SQL error: %s", sqlite3_errmsg(db->db));
  }
}

/*
** Execute a single prepared statement until it finishes.
*/
int db_exec(struct vcs_db *db, Stmt *pStmt){
  int rc;
  while( (rc = db_step(pStmt))==SQLITE_ROW ){}
  rc = db_reset(db, pStmt);
  db_check_result(db, rc);
  return rc;
}

/*
** Execute multiple SQL statements.
*/
int db_multi_exec(struct vcs_db *db, const char *zSql, ...){
  Blob sql;
  int rc;
  va_list ap;
  char *zErr = 0;
  blob_init(&sql, 0, 0);
  va_start(ap, zSql);
  blob_vappendf(db, &sql, zSql, ap);
  va_end(ap);
  rc = sqlite3_exec(db->db, blob_buffer(&sql), 0, 0, &zErr);
  if( rc!=SQLITE_OK ){
    db_err(db, "%s\n%s", zErr, blob_buffer(&sql));
  }
  blob_reset(db, &sql);
  return rc;
}

/*
** Execute a query and return a single integer value.
*/
i64 db_int64(struct vcs_db *db, i64 iDflt, const char *zSql, ...){
  va_list ap;
  Stmt s;
  i64 rc;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)!=SQLITE_ROW ){
    rc = iDflt;
  }else{
    rc = db_column_int64(&s, 0);
  }
  db_finalize(db, &s);
  return rc;
}
int db_int(struct vcs_db *db, int iDflt, const char *zSql, ...){
  va_list ap;
  Stmt s;
  int rc;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)!=SQLITE_ROW ){
    rc = iDflt;
  }else{
    rc = db_column_int(&s, 0);
  }
  db_finalize(db, &s);
  return rc;
}

/*
** Return TRUE if the query would return 1 or more rows.  Return
** FALSE if the query result would be an empty set.
*/
int db_exists(struct vcs_db *db, const char *zSql, ...){
  va_list ap;
  Stmt s;
  int rc;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)!=SQLITE_ROW ){
    rc = 0;
  }else{
    rc = 1;
  }
  db_finalize(db, &s);
  return rc;
}


/*
** Execute a query and return a floating-point value.
*/
double db_double(struct vcs_db *db, double rDflt, const char *zSql, ...){
  va_list ap;
  Stmt s;
  double r;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)!=SQLITE_ROW ){
    r = rDflt;
  }else{
    r = db_column_double(&s, 0);
  }
  db_finalize(db, &s);
  return r;
}

/*
** Execute a query and append the first column of the first row
** of the result set to blob given in the first argument.
*/
void db_blob(struct vcs_db *db, Blob *pResult, const char *zSql, ...){
  va_list ap;
  Stmt s;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)==SQLITE_ROW ){
    blob_append(db, pResult, sqlite3_column_blob(s.pStmt, 0),
                         sqlite3_column_bytes(s.pStmt, 0));
  }
  db_finalize(db, &s);
}

/*
** Execute a query.  Return the first column of the first row
** of the result set as a string.  Space to hold the string is
** obtained from malloc().  If the result set is empty, return
** zDefault instead.
*/
char *db_text(struct vcs_db *db, char *zDefault, const char *zSql, ...){
  va_list ap;
  Stmt s;
  char *z;
  va_start(ap, zSql);
  db_vprepare(db, &s, 0, zSql, ap);
  va_end(ap);
  if( db_step(&s)==SQLITE_ROW ){
    z = mprintf(db, "%s", sqlite3_column_text(s.pStmt, 0));
  }else if( zDefault ){
    z = mprintf(db, "%s", zDefault);
  }else{
    z = 0;
  }
  db_finalize(db, &s);
  return z;
}

#if defined(_WIN32)
extern char *sqlite3_win32_mbcs_to_utf8(const char*);
#endif

/*
** Initialize a new database file with the given schema.  If anything
** goes wrong, call db_err() to exit.
*/
void db_init_database(
  struct vcs_db *vdb, 
  const char *zFileName,   /* Name of database file to create */
  const char *zSchema,     /* First part of schema */
  ...                      /* Additional SQL to run.  Terminate with NULL. */
){
  sqlite3 *db;
  int rc;
  const char *zSql;
  va_list ap;

#if defined(_WIN32)
  zFileName = sqlite3_win32_mbcs_to_utf8(zFileName);
#endif
  rc = sqlite3_open(zFileName, &db);
  if( rc!=SQLITE_OK ){
    db_err(vdb, sqlite3_errmsg(db));
  }
  sqlite3_busy_timeout(db, 5000);
  sqlite3_exec(db, "BEGIN EXCLUSIVE", 0, 0, 0);
  rc = sqlite3_exec(db, zSchema, 0, 0, 0);
  if( rc!=SQLITE_OK ){
    db_err(vdb, sqlite3_errmsg(db));
  }
  va_start(ap, zSchema);
  while( (zSql = va_arg(ap, const char*))!=0 ){
    rc = sqlite3_exec(db, zSql, 0, 0, 0);
    if( rc!=SQLITE_OK ){
      db_err(vdb, sqlite3_errmsg(db));
    }
  }
  va_end(ap);
  sqlite3_exec(db, "COMMIT", 0, 0, 0);
  sqlite3_close(db);
}

/*
** Open a database file.  Return a pointer to the new database
** connection.  An error results in process abort.
*/
static sqlite3 *openDatabase(struct vcs_db *vdb, const char *zDbName){
  int rc;
  const char *zVfs;
  sqlite3 *db;

  zVfs = getenv("FOSSIL_VFS");
#if defined(_WIN32)
  zDbName = sqlite3_win32_mbcs_to_utf8(zDbName);
#endif
  rc = sqlite3_open_v2(
       zDbName, &db,
       SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
       zVfs
  );
  if( rc!=SQLITE_OK ){
    db_err(vdb, sqlite3_errmsg(db));
  }
  sqlite3_busy_timeout(db, 5000); 
  sqlite3_wal_autocheckpoint(db, 1);  /* Set to checkpoint frequently */
  return db;
}


/*
** zDbName is the name of a database file.  If no other database
** file is open, then open this one.  If another database file is
** already open, then attach zDbName using the name zLabel.
*/
static void db_open_or_attach(struct vcs_db *db, const char *zDbName, const char *zLabel){
  if( !db->db ){
    db->db = openDatabase(db, zDbName);
    db->zMainDbType = zLabel;
    db_connection_init(db);
  }else{
#if defined(_WIN32)
    zDbName = sqlite3_win32_mbcs_to_utf8(zDbName);
#endif
    db_multi_exec(db, "ATTACH DATABASE %Q AS %s", zDbName, zLabel);
  }
}

/*
** Open the user database in "~/.fossil".  Create the database anew if
** it does not already exist.
**
** If the useAttach flag is 0 (the usual case) then the user database is
** opened on a separate database connection db->dbConfig.  This prevents
** the ~/.fossil database from becoming locked on long check-in or sync
** operations which hold an exclusive transaction.  In a few cases, though,
** it is convenient for the ~/.fossil to be attached to the main database
** connection so that we can join between the various databases.  In that
** case, invoke this routine with useAttach as 1.
*/
void db_open_config(struct vcs_db *db, int useAttach){
  char *zDbName;
  const char *zHome;
  if( db->configOpen ) return;
#if defined(_WIN32)
  zHome = getenv("LOCALAPPDATA");
  if( zHome==0 ){
    zHome = getenv("APPDATA");
    if( zHome==0 ){
      zHome = getenv("HOMEPATH");
    }
  }
  if( zHome==0 ){
    geomvcs_fatal(db, "cannot locate home directory - "
                "please set the HOMEPATH environment variable");
  }
#else
  zHome = getenv("HOME");
  if( zHome==0 ){
    geomvcs_fatal(db, "cannot locate home directory - "
                 "please set the HOME environment variable");
  }
#endif
  if( file_isdir(db, zHome)!=1 ){
    geomvcs_fatal(db, "invalid home directory: %s", zHome);
  }
#ifndef _WIN32
  if( access(zHome, W_OK) ){
    geomvcs_fatal(db, "home directory %s must be writeable", zHome);
  }
#endif
  db->zHome = mprintf(db, "%/", zHome);
#if defined(_WIN32)
  /* . filenames give some window systems problems and many apps problems */
  zDbName = mprintf(db, "%//_fossil", zHome);
#else
  zDbName = mprintf(db, "%s/.fossil", zHome);
#endif
  if( file_size(zDbName)<1024*3 ){
    db_init_database(db, zDbName, zConfigSchema, (char*)0);
  }
  db->useAttach = useAttach;
  if( useAttach ){
    db_open_or_attach(db, zDbName, "configdb");
    db->dbConfig = 0;
  }else{
    db->dbConfig = openDatabase(db, zDbName);
  }
  db->configOpen = 1;
  free(zDbName);
}

/*
** If zDbName is a valid local database file, open it and return
** true.  If it is not a valid local database file, return 0.
*/
static int isValidLocalDb(struct vcs_db *db, const char *zDbName){
  i64 lsize;
  int rc;
  sqlite3_stmt *pStmt;

  if( access(zDbName, F_OK) ) return 0;
  lsize = file_size(zDbName);
  if( lsize%1024!=0 || lsize<4096 ) return 0;
  db_open_or_attach(db, zDbName, "localdb");
  db->localOpen = 1;
  db_open_config(db, 0);
  db_open_repository(db, 0);

  /* If the "isexe" column is missing from the vfile table, then
  ** add it now.   This code added on 2010-03-06.  After all users have
  ** upgraded, this code can be safely deleted. 
  */
  rc = sqlite3_prepare(db->db, "SELECT isexe FROM vfile", -1, &pStmt, 0);
  nPrepare++;
  sqlite3_finalize(pStmt);
  if( rc==SQLITE_ERROR ){
    sqlite3_exec(db->db, "ALTER TABLE vfile ADD COLUMN isexe BOOLEAN", 0, 0, 0);
  }

#if 0
  /* If the "mtime" column is missing from the vfile table, then
  ** add it now.   This code added on 2008-12-06.  After all users have
  ** upgraded, this code can be safely deleted. 
  */
  rc = sqlite3_prepare(db->db, "SELECT mtime FROM vfile", -1, &pStmt, 0);
  sqlite3_finalize(pStmt);
  if( rc==SQLITE_ERROR ){
    sqlite3_exec(db->db, "ALTER TABLE vfile ADD COLUMN mtime INTEGER", 0, 0, 0);
  }
#endif

#if 0
  /* If the "origname" column is missing from the vfile table, then
  ** add it now.   This code added on 2008-11-09.  After all users have
  ** upgraded, this code can be safely deleted. 
  */
  rc = sqlite3_prepare(db->db, "SELECT origname FROM vfile", -1, &pStmt, 0);
  sqlite3_finalize(pStmt);
  if( rc==SQLITE_ERROR ){
    sqlite3_exec(db->db, "ALTER TABLE vfile ADD COLUMN origname TEXT", 0, 0, 0);
  }
#endif

  return 1;
}

/*
** Locate the root directory of the local repository tree.  The root
** directory is found by searching for a file named "_FOSSIL_" or ".fos"
** that contains a valid repository database.
**
** If no valid _FOSSIL_ or .fos file is found, we move up one level and 
** try again. Once the file is found, the db->zLocalRoot variable is set
** to the root of the repository tree and this routine returns 1.  If
** no database is found, then this routine return 0.
**
** This routine always opens the user database regardless of whether or
** not the repository database is found.  If the _FOSSIL_ or .fos file
** is found, it is attached to the open database connection too.
*/
int db_open_local(struct vcs_db *db){
  int i, n;
  char zPwd[2000];
  char *zPwdConv;
  static const char *aDbName[] = { "/_FOSSIL_", "/.fos" };
  
  if( db->localOpen) return 1;
  if( getcwd(zPwd, sizeof(zPwd)-20)==0 ){
    db_err(db, "pwd too big: max %d", sizeof(zPwd)-20);
  }
  n = strlen(zPwd);
  zPwdConv = mprintf(db, "%/", zPwd);
  strncpy(zPwd, zPwdConv, 2000-20);
  free(zPwdConv);
  while( n>0 ){
    if( access(zPwd, W_OK) ) break;
    for(i=0; i<sizeof(aDbName)/sizeof(aDbName[0]); i++){
      sqlite3_snprintf(sizeof(zPwd)-n, &zPwd[n], "%s", aDbName[i]);
      if( isValidLocalDb(db, zPwd) ){
        /* Found a valid checkout database file */
        zPwd[n] = 0;
        while( n>1 && zPwd[n-1]=='/' ){
          n--;
          zPwd[n] = 0;
        }
        db->zLocalRoot = mprintf(db, "%s/", zPwd);
        return 1;
      }
    }
    n--;
    while( n>0 && zPwd[n]!='/' ){ n--; }
    while( n>0 && zPwd[n-1]=='/' ){ n--; }
    zPwd[n] = 0;
  }

  /* A checkout database file could not be found */
  return 0;
}

/*
** Open the repository database given by zDbName.  If zDbName==NULL then
** get the name from the already open local database.
*/
void db_open_repository(struct vcs_db *db, const char *zDbName){
  if( db->repositoryOpen ) return;
  if( zDbName==0 ){
    if( db->localOpen ){
      zDbName = db_lget(db, "repository", 0);
    }
    if( zDbName==0 ){
      db_err(db, "unable to find the name of a repository database");
    }
  }
  if( access(zDbName, R_OK) || file_size(zDbName)<1024 ){
    if( access(zDbName, 0) ){
      geomvcs_panic(db, "repository does not exist or"
                   " is in an unreadable directory: %s", zDbName);
    }else if( access(zDbName, R_OK) ){
      geomvcs_panic(db, "read permission denied for repository %s", zDbName);
    }else{
      geomvcs_panic(db, "not a valid repository: %s", zDbName);
    }
  }
  db_open_or_attach(db, zDbName, "repository");
  db->repositoryOpen = 1;
  db->zRepositoryName = mprintf(db, "%s", zDbName);
}

/*
** Try to find the repository and open it.  Use the -R or --repository
** option to locate the repository.  If no such option is available, then
** use the repository of the open checkout if there is one.
**
** Error out if the repository cannot be opened.
*/
void db_find_and_open_repository(struct vcs_db *db, int bFlags, int nArgUsed){
  const char *zRep = find_option("repository", "R", 1);
  if( zRep==0 && nArgUsed && db->argc==nArgUsed+1 ){
    zRep = db->argv[nArgUsed];
  }
  if( zRep==0 ){
    if( db_open_local(db)==0 ){
      goto rep_not_found;
    }
    zRep = db_lget(db, "repository", 0);
    if( zRep==0 ){
      goto rep_not_found;
    }
  }
  db_open_repository(db, zRep);
  if( db->repositoryOpen ){
    if( (bFlags & OPEN_ANY_SCHEMA)==0 ) db_verify_schema(db);
    return;
  }
rep_not_found:
  if( (bFlags & OPEN_OK_NOT_FOUND)==0 ){
    geomvcs_fatal(db, "use --repository or -R to specify the repository database");
  }
}

/*
** Return the name of the database "localdb", "configdb", or "repository".
*/
const char *db_name(struct vcs_db *db, const char *zDb){
  assert( strcmp(zDb,"localdb")==0
       || strcmp(zDb,"configdb")==0
       || strcmp(zDb,"repository")==0 );
  if( strcmp(zDb, db->zMainDbType)==0 ) zDb = "main";
  return zDb;
}

/*
** Return TRUE if the schema is out-of-date
*/
int db_schema_is_outofdate(struct vcs_db *db){
  return db_exists(db, "SELECT 1 FROM config"
                   " WHERE name='aux-schema'"
                   "   AND value<>'%s'", AUX_SCHEMA);
}

/*
** Verify that the repository schema is correct.  If it is not correct,
** issue a fatal error and die.
*/
void db_verify_schema(struct vcs_db *db){
  if( db_schema_is_outofdate(db) ){
    geomvcs_warning(db, "incorrect repository schema version");
    geomvcs_warning(db, "you have version \"%s\" but you need version \"%s\"",
          db_get(db, "aux-schema",0), AUX_SCHEMA);
    geomvcs_fatal(db, "run \"fossil rebuild\" to fix this problem");
  }
}

/*
** Open the local database.  If unable, exit with an error.
*/
void db_must_be_within_tree(struct vcs_db *db){
  if( db_open_local(db)==0 ){
    geomvcs_fatal(db, "not within an open checkout");
  }
  db_open_repository(db, 0);
  db_verify_schema(db);
}

/*
** Close the database connection.
**
** Check for unfinalized statements and report errors if the reportErrors
** argument is true.  Ignore unfinalized statements when false.
*/
void db_close(struct vcs_db *db, int reportErrors){
  sqlite3_stmt *pStmt;
  if( db->db==0 ) return;
  if( db->fSqlStats ){
    int cur, hiwtr;
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_LOOKASIDE_USED, &cur, &hiwtr, 0);
    fprintf(stderr, "-- LOOKASIDE_USED         %10d %10d\n", cur, hiwtr);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_LOOKASIDE_HIT, &cur, &hiwtr, 0);
    fprintf(stderr, "-- LOOKASIDE_HIT                     %10d\n", hiwtr);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE, &cur,&hiwtr,0);
    fprintf(stderr, "-- LOOKASIDE_MISS_SIZE               %10d\n", hiwtr);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL, &cur,&hiwtr,0);
    fprintf(stderr, "-- LOOKASIDE_MISS_FULL               %10d\n", hiwtr);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_CACHE_USED, &cur, &hiwtr, 0);
    fprintf(stderr, "-- CACHE_USED             %10d\n", cur);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_SCHEMA_USED, &cur, &hiwtr, 0);
    fprintf(stderr, "-- SCHEMA_USED            %10d\n", cur);
    sqlite3_db_status(db->db, SQLITE_DBSTATUS_STMT_USED, &cur, &hiwtr, 0);
    fprintf(stderr, "-- STMT_USED              %10d\n", cur);
    sqlite3_status(SQLITE_STATUS_MEMORY_USED, &cur, &hiwtr, 0);
    fprintf(stderr, "-- MEMORY_USED            %10d %10d\n", cur, hiwtr);
    sqlite3_status(SQLITE_STATUS_MALLOC_SIZE, &cur, &hiwtr, 0);
    fprintf(stderr, "-- MALLOC_SIZE                       %10d\n", hiwtr);
    sqlite3_status(SQLITE_STATUS_MALLOC_COUNT, &cur, &hiwtr, 0);
    fprintf(stderr, "-- MALLOC_COUNT           %10d %10d\n", cur, hiwtr);
    sqlite3_status(SQLITE_STATUS_PAGECACHE_OVERFLOW, &cur, &hiwtr, 0);
    fprintf(stderr, "-- PCACHE_OVFLOW          %10d %10d\n", cur, hiwtr);
    fprintf(stderr, "-- prepared statements    %10d\n", nPrepare);
  }
  while( pAllStmt ){
    db_finalize(db, pAllStmt);
  }
  db_end_transaction(db, 1);
  pStmt = 0;
  if( reportErrors ){
    while( (pStmt = sqlite3_next_stmt(db->db, pStmt))!=0 ){
      geomvcs_warning(db, "unfinalized SQL statement: [%s]", sqlite3_sql(pStmt));
    }
  }
  db->repositoryOpen = 0;
  db->localOpen = 0;
  db->configOpen = 0;
  sqlite3_wal_checkpoint(db->db, 0);
  sqlite3_close(db->db);
  db->db = 0;
  if( db->dbConfig ){
    sqlite3_close(db->dbConfig);
    db->dbConfig = 0;
  }
}


/*
** Create a new empty repository database with the given name.
**
** Only the schema is initialized.  The required VAR tables entries
** are not set by this routine and must be set separately in order
** to make the new file a valid database.
*/
void db_create_repository(struct vcs_db *db, const char *zFilename){
  db_init_database(
     db,
     zFilename,
     zRepositorySchema1,
     zRepositorySchema2,
     (char*)0
  );
  isNewRepo = 1;
}

/*
** Create the default user accounts in the USER table.
*/
void db_create_default_users(struct vcs_db *db, int setupUserOnly, const char *zDefaultUser){
  const char *zUser;
  zUser = db_get(db, "default-user", 0);
  if( zUser==0 ){
    zUser = zDefaultUser;
  }
  if( zUser==0 ){
#if defined(_WIN32)
    zUser = getenv("USERNAME");
#else
    zUser = getenv("USER");
#endif
  }
  if( zUser==0 ){
    zUser = "root";
  }
  db_multi_exec(
     db,
     "INSERT INTO user(login, pw, cap, info)"
     "VALUES(%Q,lower(hex(randomblob(3))),'s','')", zUser
  );
  if( !setupUserOnly ){
    db_multi_exec(
       db,
       "INSERT INTO user(login,pw,cap,info)"
       "   VALUES('anonymous',hex(randomblob(8)),'hmncz','Anon');"
       "INSERT INTO user(login,pw,cap,info)"
       "   VALUES('nobody','','gjor','Nobody');"
       "INSERT INTO user(login,pw,cap,info)"
       "   VALUES('developer','','dei','Dev');"
       "INSERT INTO user(login,pw,cap,info)"
       "   VALUES('reader','','kptw','Reader');"
    );
  }
}

/*
** Fill an empty repository database with the basic information for a
** repository. This function is shared between 'create_repository_cmd'
** ('new') and 'reconstruct_cmd' ('reconstruct'), both of which create
** new repositories.
**
** The zInitialDate parameter determines the date of the initial check-in
** that is automatically created.  If zInitialDate is 0 then no initial
** check-in is created. The makeServerCodes flag determines whether or
** not server and project codes are invented for this repository.
*/
void db_initial_setup(
  struct vcs_db *db,
  const char *zInitialDate,    /* Initial date of repository. (ex: "now") */
  const char *zDefaultUser,    /* Default user for the repository */
  int makeServerCodes          /* True to make new server & project codes */
){
  char *zDate;
  Blob hash;
  Blob manifest;

  db_set(db, "content-schema", CONTENT_SCHEMA, 0);
  db_set(db, "aux-schema", AUX_SCHEMA, 0);
  if( makeServerCodes ){
    db_multi_exec(
      db,
      "INSERT INTO config(name,value)"
      " VALUES('server-code', lower(hex(randomblob(20))));"
      "INSERT INTO config(name,value)"
      " VALUES('project-code', lower(hex(randomblob(20))));"
    );
  }
  if( !db_is_global(db, "autosync") ) db_set_int(db, "autosync", 1, 0);
  if( !db_is_global(db, "localauth") ) db_set_int(db, "localauth", 0, 0);
  db_create_default_users(db, 0, zDefaultUser);
  user_select();

  if( zInitialDate ){
    int rid;
    blob_zero(&manifest);
    blob_appendf(db, &manifest, "C initial\\sempty\\scheck-in\n");
    zDate = date_in_standard_format(zInitialDate);
    blob_appendf(db, &manifest, "D %s\n", zDate);
    blob_appendf(db, &manifest, "P\n");
    md5sum_init();
    blob_appendf(db, &manifest, "R %s\n", md5sum_finish(db, 0));
    blob_appendf(db, &manifest, "T *branch * trunk\n");
    blob_appendf(db, &manifest, "T *sym-trunk *\n");
    blob_appendf(db, &manifest, "U %F\n", db->zLogin);
    md5sum_blob(db, &manifest, &hash);
    blob_appendf(db, &manifest, "Z %b\n", &hash);
    blob_reset(db, &hash);
    rid = content_put(&manifest);
    manifest_crosslink(rid, &manifest);
  }
}

/*
** SQL functions for debugging.
**
** The print() function writes its arguments on stdout, but only
** if the -sqlprint command-line option is turned on.
*/
static void db_sql_print(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  int i;
  struct vcs_db *db = (struct vcs_db *)sqlite3_user_data(context); 
  if( db->fSqlPrint ){
    for(i=0; i<argc; i++){
      char c = i==argc-1 ? '\n' : ' ';
      printf("%s%c", sqlite3_value_text(argv[i]), c);
    }
  }
}
static void db_sql_trace(void *notUsed, const char *zSql){
  int n = strlen(zSql);
  fprintf(stderr, "%s%s\n", zSql, (n>0 && zSql[n-1]==';') ? "" : ";");
}

/*
** Implement the user() SQL function.  user() takes no arguments and
** returns the user ID of the current user.
*/
static void db_sql_user(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  struct vcs_db *db = (struct vcs_db *)sqlite3_user_data(context); 
  if( db->zLogin!=0 ){
    sqlite3_result_text(context, db->zLogin, -1, SQLITE_STATIC);
  }
}

/*
** Implement the cgi() SQL function.  cgi() takes a an argument which is
** a name of CGI query parameter. The value of that parameter is returned, 
** if available. optional second argument will be returned if the first
** doesn't exist as a CGI parameter.
*/
static void db_sql_cgi(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char* zP;
  if( argc!=1 && argc!=2 ) return;
  zP = P((const char*)sqlite3_value_text(argv[0]));
  if( zP ){
    sqlite3_result_text(context, zP, -1, SQLITE_STATIC);
  }else if( argc==2 ){
    zP = (const char*)sqlite3_value_text(argv[1]);
    if( zP ) sqlite3_result_text(context, zP, -1, SQLITE_TRANSIENT);
  }
}

/*
** This is used by the [commit] command.
**
** Return true if either:
**
**     a) Global.aCommitFile is NULL, or
**     b) Global.aCommitFile contains the integer passed as an argument.
**
** Otherwise return false.
*/
static void file_is_selected(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  struct vcs_db *db = (struct vcs_db *)sqlite3_user_data(context); 
  assert(argc==1);
  if( db->aCommitFile ){
    int iId = sqlite3_value_int(argv[0]);
    int ii;
    for(ii=0; db->aCommitFile[ii]; ii++){
      if( iId==db->aCommitFile[ii] ){
        sqlite3_result_int(context, 1);
        return;
      }
    }
    sqlite3_result_int(context, 0);
  }else{
    sqlite3_result_int(context, 1);
  }
}

/*
** Convert the input string into an SHA1.  Make a notation in the
** CONCEALED table so that the hash can be undo using the db_reveal()
** function at some later time.
**
** The value returned is stored in static space and will be overwritten
** on subsequent calls.
**
** If zContent is already a well-formed SHA1 hash, then return a copy
** of that hash, not a hash of the hash.
**
** The CONCEALED table is meant to obscure email addresses.  Every valid
** email address will contain a "@" character and "@" is not valid within
** an SHA1 hash so there is no chance that a valid email address will go
** unconcealed.
*/
char *db_conceal(struct vcs_db *db, const char *zContent, int n){
  static char zHash[42];
  Blob out;
  if( n==40 && validate16(zContent, n) ){
    memcpy(zHash, zContent, n);
    zHash[n] = 0;
  }else{
    sha1sum_step_text(zContent, n);
    sha1sum_finish(db, &out);
    sqlite3_snprintf(sizeof(zHash), zHash, "%s", blob_str(db, &out));
    blob_reset(db, &out);
    db_multi_exec(
       db,
       "INSERT OR IGNORE INTO concealed VALUES(%Q,%#Q)",
       zHash, n, zContent
    );
  }
  return zHash;
}

/*
** Attempt to look up the input in the CONCEALED table.  If found,
** and if the okRdAddr permission is enabled then return the
** original value for which the input is a hash.  If okRdAddr is
** false or if the lookup fails, return the original string content.
**
** In either case, the string returned is stored in space obtained
** from malloc and should be freed by the calling function.
*/
char *db_reveal(struct vcs_db *db, const char *zKey){
  char *zOut;
  if( db->okRdAddr ){
    zOut = db_text(db, 0, "SELECT content FROM concealed WHERE hash=%Q", zKey);
  }else{
    zOut = 0;
  }
  if( zOut==0 ){
    zOut = mprintf(db, "%s", zKey);
  }
  return zOut;
}

/*
** This function registers auxiliary functions when the SQLite
** database connection is first established.
*/
void db_connection_init(struct vcs_db *db){
  sqlite3_exec(db->db, "PRAGMA foreign_keys=OFF;", 0, 0, 0);
  sqlite3_create_function(db->db, "user", 0, SQLITE_ANY, (void *)db, db_sql_user, 0, 0);
  sqlite3_create_function(db->db, "cgi", 1, SQLITE_ANY, (void *)db, db_sql_cgi, 0, 0);
  sqlite3_create_function(db->db, "cgi", 2, SQLITE_ANY, (void *)db, db_sql_cgi, 0, 0);
  sqlite3_create_function(db->db, "print", -1, SQLITE_UTF8, (void *)db, db_sql_print,0,0);
  sqlite3_create_function(
    db->db, "file_is_selected", 1, SQLITE_UTF8, (void *)db, file_is_selected,0,0
  );
  if( db->fSqlTrace ){
    sqlite3_trace(db->db, db_sql_trace, 0);
  }
}

/*
** Return true if the string zVal represents "true" (or "false").
*/
int is_truth(const char *zVal){
  static const char *azOn[] = { "on", "yes", "true", "1" };
  int i;
  for(i=0; i<sizeof(azOn)/sizeof(azOn[0]); i++){
    if( geomvcs_strcmp(zVal,azOn[i])==0 ) return 1;
  }
  return 0;
}
int is_false(const char *zVal){
  static const char *azOff[] = { "off", "no", "false", "0" };
  int i;
  for(i=0; i<sizeof(azOff)/sizeof(azOff[0]); i++){
    if( geomvcs_strcmp(zVal,azOff[i])==0 ) return 1;
  }
  return 0;
}

/*
** Swap the db->db and db->dbConfig connections so that the various db_* routines
** work on the ~/.fossil database instead of on the repository database.
** Be sure to swap them back after doing the operation.
**
** If db->useAttach that means the ~/.fossil database was opened with
** the useAttach flag set to 1.  In that case no connection swap is required
** so this routine is a no-op.
*/
void db_swap_connections(struct vcs_db *db){
  if( !db->useAttach ){
    sqlite3 *dbTemp = db->db;
    db->db = db->dbConfig;
    db->dbConfig = dbTemp;
  }
}

/*
** Get and set values from the CONFIG, GLOBAL_CONFIG and VVAR table in the
** repository and local databases.
*/
char *db_get(struct vcs_db *db, const char *zName, char *zDefault){
  char *z = 0;
  if( db->repositoryOpen ){
    z = db_text(db, 0, "SELECT value FROM config WHERE name=%Q", zName);
  }
  if( z==0 && db->configOpen ){
    db_swap_connections(db);
    z = db_text(db, 0, "SELECT value FROM global_config WHERE name=%Q", zName);
    db_swap_connections(db);
  }
  if( z==0 ){
    z = zDefault;
  }
  return z;
}
void db_set(struct vcs_db *db, const char *zName, const char *zValue, int globalFlag){
  db_begin_transaction(db);
  if( globalFlag ){
    db_swap_connections(db);
    db_multi_exec(db, "REPLACE INTO global_config(name,value) VALUES(%Q,%Q)",
                   zName, zValue);
    db_swap_connections(db);
  }else{
    db_multi_exec(db, "REPLACE INTO config(name,value) VALUES(%Q,%Q)",
                   zName, zValue);
  }
  if( globalFlag && db->repositoryOpen ){
    db_multi_exec(db, "DELETE FROM config WHERE name=%Q", zName);
  }
  db_end_transaction(db, 0);
}
void db_unset(struct vcs_db *db, const char *zName, int globalFlag){
  db_begin_transaction(db);
  if( globalFlag ){
    db_swap_connections(db);
    db_multi_exec(db, "DELETE FROM global_config WHERE name=%Q", zName);
    db_swap_connections(db);
  }else{
    db_multi_exec(db, "DELETE FROM config WHERE name=%Q", zName);
  }
  if( globalFlag && db->repositoryOpen ){
    db_multi_exec(db, "DELETE FROM config WHERE name=%Q", zName);
  }
  db_end_transaction(db, 0);
}
int db_is_global(struct vcs_db *db, const char *zName){
  int rc = 0;
  if( db->configOpen ){
    db_swap_connections(db);
    rc = db_exists(db, "SELECT 1 FROM global_config WHERE name=%Q", zName);
    db_swap_connections(db);
  }
  return rc;
}
int db_get_int(struct vcs_db *db, const char *zName, int dflt){
  int v = dflt;
  int rc;
  if( db->repositoryOpen ){
    Stmt q;
    db_prepare(db, &q, "SELECT value FROM config WHERE name=%Q", zName);
    rc = db_step(&q);
    if( rc==SQLITE_ROW ){
      v = db_column_int(&q, 0);
    }
    db_finalize(db, &q);
  }else{
    rc = SQLITE_DONE;
  }
  if( rc==SQLITE_DONE && db->configOpen ){
    db_swap_connections(db);
    v = db_int(db, dflt, "SELECT value FROM global_config WHERE name=%Q", zName);
    db_swap_connections(db);
  }
  return v;
}
void db_set_int(struct vcs_db *db, const char *zName, int value, int globalFlag){
  if( globalFlag ){
    db_swap_connections(db);
    db_multi_exec(db, "REPLACE INTO global_config(name,value) VALUES(%Q,%d)",
                  zName, value);
    db_swap_connections(db);
  }else{
    db_multi_exec(db, "REPLACE INTO config(name,value) VALUES(%Q,%d)",
                  zName, value);
  }
  if( globalFlag && db->repositoryOpen ){
    db_multi_exec(db, "DELETE FROM config WHERE name=%Q", zName);
  }
}
int db_get_boolean(struct vcs_db *db, const char *zName, int dflt){
  char *zVal = db_get(db, zName, dflt ? "on" : "off");
  if( is_truth(zVal) ) return 1;
  if( is_false(zVal) ) return 0;
  return dflt;
}
char *db_lget(struct vcs_db *db, const char *zName, char *zDefault){
  return db_text(db, (char*)zDefault,
                 "SELECT value FROM vvar WHERE name=%Q", zName);
}
void db_lset(struct vcs_db *db, const char *zName, const char *zValue){
  db_multi_exec(db, "REPLACE INTO vvar(name,value) VALUES(%Q,%Q)", zName, zValue);
}
int db_lget_int(struct vcs_db *db, const char *zName, int dflt){
  return db_int(db, dflt, "SELECT value FROM vvar WHERE name=%Q", zName);
}
void db_lset_int(struct vcs_db *db, const char *zName, int value){
  db_multi_exec(db, "REPLACE INTO vvar(name,value) VALUES(%Q,%d)", zName, value);
}

/*
** Record the name of a local repository in the global_config() database.
** The repository filename %s is recorded as an entry with a "name" field
** of the following form:
**
**       repo:%s
**
** The value field is set to 1.
*/
void db_record_repository_filename(struct vcs_db *db, const char *zName){
  Blob full;
  if( zName==0 ){
    if( !db->localOpen ) return;
    zName = db_lget(db, "repository", 0);
  }
  file_canonical_name(db, zName, &full);
  db_swap_connections(db);
  db_multi_exec(
     db, 
     "INSERT OR IGNORE INTO global_config(name,value)"
     "VALUES('repo:%q',1)",
     blob_str(db, &full)
  );
  db_swap_connections(db);
  blob_reset(db, &full);
}

/*
** Print the value of a setting named zName
*/
static void print_setting(struct vcs_db *db, const char *zName){
  Stmt q;
  if( db->repositoryOpen ){
    db_prepare(db, &q,
       "SELECT '(local)', value FROM config WHERE name=%Q"
       " UNION ALL "
       "SELECT '(global)', value FROM global_config WHERE name=%Q",
       zName, zName
    );
  }else{
    db_prepare(db, &q,
      "SELECT '(global)', value FROM global_config WHERE name=%Q",
      zName
    );
  }
  if( db_step(&q)==SQLITE_ROW ){
    printf("%-20s %-8s %s\n", zName, db_column_text(&q, 0),
        db_column_text(&q, 1));
  }else{
    printf("%-20s\n", zName);
  }
  db_finalize(db, &q);
}


/*
** define all settings, which can be controlled via the set/unset
** command. var is the name of the internal configuration name for db_(un)set.
** If var is 0, the settings name is used.
** width is the length for the edit field on the behavior page, 0
** is used for on/off checkboxes.
** The behaviour page doesn't use a special layout. It lists all
** set-commands and displays the 'set'-help as info.
*/
struct stControlSettings const ctrlSettings[] = {
  { "access-log",    0,                0, "off"                 },
  { "auto-captcha",  "autocaptcha",    0, "on"                  },
  { "auto-shun",     0,                0, "on"                  },
  { "autosync",      0,                0, "on"                  },
  { "binary-glob",   0,               32, ""                    },
  { "clearsign",     0,                0, "off"                 },
  { "crnl-glob",     0,               16, ""                    },
  { "default-perms", 0,               16, "u"                   },
  { "diff-command",  0,               16, ""                    },
  { "dont-push",     0,                0, "off"                 },
  { "editor",        0,               16, ""                    },
  { "gdiff-command", 0,               16, "gdiff"               },
  { "gmerge-command",0,               40, ""                    },
  { "ignore-glob",   0,               40, ""                    },
  { "http-port",     0,               16, "8080"                },
  { "localauth",     0,                0, "off"                 },
  { "main-branch",   0,               40, "trunk"               },
  { "manifest",      0,                0, "off"                 },
  { "max-upload",    0,               25, "250000"              },
  { "mtime-changes", 0,                0, "on"                  },
  { "pgp-command",   0,               32, "gpg --clearsign -o " },
  { "proxy",         0,               32, "off"                 },
  { "repo-cksum",    0,                0, "on"                  },
  { "self-register", 0,                0, "off"                 },
  { "ssh-command",   0,               32, ""                    },
  { "web-browser",   0,               32, ""                    },
  { 0,0,0,0 }
};

/*
** The input in a a timespan measured in days.  Return a string which
** describes that timespan in units of seconds, minutes, hours, days,
** or years, depending on its duration.
*/
char *db_timespan_name(double rSpan){
  if( rSpan<0 ) rSpan = -rSpan;
  rSpan *= 24.0*3600.0;  /* Convert units to seconds */
  if( rSpan<120.0 ){
    return sqlite3_mprintf("%.1f seconds", rSpan);
  }
  rSpan /= 60.0;         /* Convert units to minutes */
  if( rSpan<90.0 ){
    return sqlite3_mprintf("%.1f minutes", rSpan);
  }
  rSpan /= 60.0;         /* Convert units to hours */
  if( rSpan<=48.0 ){
    return sqlite3_mprintf("%.1f hours", rSpan);
  }
  rSpan /= 24.0;         /* Convert units to days */
  if( rSpan<=365.0 ){
    return sqlite3_mprintf("%.1f days", rSpan);
  }
  rSpan /= 356.24;         /* Convert units to years */
  return sqlite3_mprintf("%.1f years", rSpan);
}

