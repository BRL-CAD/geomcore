#include "geomvcs/sqlite3.h"

/*
** An single SQL statement is represented as an instance of the following
** structure.
*/
typedef struct Stmt Stmt;
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};

/*
** Flags for the db_find_and_open_repository() function.
*/
#define OPEN_OK_NOT_FOUND    0x001      /* Do not error out if not found */ 
#define OPEN_ANY_SCHEMA      0x002      /* Do not error if schema is wrong */


struct stControlSettings {
  char const *name;     /* Name of the setting */
  char const *var;      /* Internal variable name used by db_set() */
  int width;            /* Width of display.  0 for boolean values */
  char const *def;      /* Default value */
};

void db_force_rollback(struct vcs_db *db);
void db_check_result(struct vcs_db *db, int rc);
int db_multi_exec(struct vcs_db *db, const char *zSql, ...);
char *db_lget(struct vcs_db *db, const char *zName, char *zDefault);
void db_open_repository(struct vcs_db *db, const char *zDbName);
void db_verify_schema(struct vcs_db *db);
void db_close(struct vcs_db *db, int reportErrors);
int db_finalize(struct vcs_db *db, Stmt *pStmt);
void db_begin_transaction(struct vcs_db *db);
void db_end_transaction(struct vcs_db *db, int rollbackFlag);
char *db_get(struct vcs_db *db, const char *zName, char *zDefault);
void db_set(struct vcs_db *db, const char *zName, const char *zValue, int globalFlag);
char *db_text(struct vcs_db *db, char *zDefault, const char *zSql, ...);
char *db_lget(struct vcs_db *db, const char *zName, char *zDefault);
void db_set_int(struct vcs_db *db, const char *zName, int value, int globalFlag);
void db_connection_init(struct vcs_db *db);
int db_int(struct vcs_db *db, int iDflt, const char *zSql, ...);
int db_prepare(struct vcs_db *db, Stmt *pStmt, const char *zFormat, ...);
int db_step(Stmt *pStmt);
int db_column_int(Stmt *pStmt, int N);
int db_bind_int(struct vcs_db *db, Stmt *pStmt, const char *zParamName, int iValue);
int db_reset(struct vcs_db *db, Stmt *pStmt);
double db_column_double(Stmt *pStmt, int N);
const char *db_column_text(Stmt *pStmt, int N);
void db_must_be_within_tree(struct vcs_db *db);
double db_double(struct vcs_db *db, double rDflt, const char *zSql, ...);
int db_static_prepare(struct vcs_db *db, Stmt *pStmt, const char *zFormat, ...);
int db_bind_text(struct vcs_db *db, Stmt *pStmt, const char *zParamName, const char *zValue);
int db_bind_double(struct vcs_db *db, Stmt *pStmt, const char *zParamName, double rValue);
int db_exists(struct vcs_db *db, const char *zSql, ...);
int db_exec(struct vcs_db *db, Stmt *pStmt);
i64 db_last_insert_rowid(struct vcs_db *db);
void db_must_be_within_tree(struct vcs_db *db);
int db_is_global(struct vcs_db *db, const char *zName);
