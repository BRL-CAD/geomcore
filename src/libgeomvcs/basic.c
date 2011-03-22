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
** This module codes the main() procedure that runs first when the
** program is invoked.
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/blob.h"
#include "geomvcs/db.h"
#include "geomvcs/printf.h"
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
** Exit.  Take care to close the database first.
*/
void geomvcs_exit(struct vcs_db *db, int rc){
  db_close(db, 1);
  exit(rc);
}

/*
** Print an error message, rollback all databases, and quit.  These
** routines never return.
*/
void geomvcs_panic(struct vcs_db *db, const char *zFormat, ...){
  char *z;
  va_list ap;
  static int once = 1;
  db->mainInFatalError = 1;
  va_start(ap, zFormat);
  z = vmprintf(db, zFormat, ap);
  va_end(ap);
  fprintf(stderr, "%s\n", z);
  db_force_rollback(db);
  geomvcs_exit(db, 1);
}

void geomvcs_fatal(struct vcs_db *db, const char *zFormat, ...){
  char *z;
  va_list ap;
  db->mainInFatalError = 1;
  va_start(ap, zFormat);
  z = vmprintf(db, zFormat, ap);
  va_end(ap);
  fprintf(stderr, "\r%s\n", z);
  db_force_rollback(db);
  geomvcs_exit(db, 1);
}

/* This routine works like geomvcs_fatal() except that if called
** recursively, the recursive call is a no-op.
**
** Use this in places where an error might occur while doing
** fatal error shutdown processing.  Unlike geomvcs_panic() and
** geomvcs_fatal() which never return, this routine might return if
** the fatal error handing is already in process.  The caller must
** be prepared for this routine to return.
*/
void geomvcs_fatal_recursive(struct vcs_db *db, const char *zFormat, ...){
  char *z;
  va_list ap;
  if( db->mainInFatalError ) return;
  db->mainInFatalError = 1;
  va_start(ap, zFormat);
  z = vmprintf(db, zFormat, ap);
  va_end(ap);
  fprintf(stderr, "\r%s\n", z);
  db_force_rollback(db);
  geomvcs_exit(db, 1);
}


/* Print a warning message */
void geomvcs_warning(struct vcs_db *db, const char *zFormat, ...){
  char *z;
  va_list ap;
  va_start(ap, zFormat);
  z = vmprintf(db, zFormat, ap);
  va_end(ap);
  fprintf(stderr, "\r%s\n", z);
}

/*
** Malloc and free routines that cannot fail
*/
void *geomvcs_malloc(struct vcs_db *db, size_t n){
  void *p = malloc(n);
  if( p==0 ) geomvcs_panic(db, "out of memory");
  return p;
}
void geomvcs_free(void *p){
  free(p);
}
void *geomvcs_realloc(struct vcs_db *db, void *p, size_t n){
  p = realloc(p, n);
  if( p==0 ) geomvcs_panic(db, "out of memory");
  return p;
}

/*
** This function implements a cross-platform "system()" interface.
*/
int geomvcs_system(const char *zOrigCmd){
  int rc;
#if defined(_WIN32)
  /* On windows, we have to put double-quotes around the entire command.
  ** Who knows why - this is just the way windows works.
  */
  char *zNewCmd = mprintf("\"%s\"", zOrigCmd);
  rc = system(zNewCmd);
  free(zNewCmd);
#else
  /* On unix, evaluate the command directly.
  */
  rc = system(zOrigCmd);
#endif 
  return rc; 
}

/*
** Like strcmp() except that it accepts NULL pointers.  NULL sorts before
** all non-NULL string pointers.
*/
int geomvcs_strcmp(const char *zA, const char *zB){
  if( zA==0 ){
    if( zB==0 ) return 0;
    return -1;
  }else if( zB==0 ){
    return +1;
  }else{
    return strcmp(zA,zB);
  }
}

/*
** Turn off any NL to CRNL translation on the stream given as an
** argument.  This is a no-op on unix but is necessary on windows.
*/
void geomvcs_binary_mode(FILE *p){
#if defined(_WIN32)
  _setmode(_fileno(p), _O_BINARY);
#endif
}



/*
** Return a name for an SQLite error code
*/
static const char *sqlite_error_code_name(int iCode){
  static char zCode[30];
  switch( iCode & 0xff ){
    case SQLITE_OK:         return "SQLITE_OK";
    case SQLITE_ERROR:      return "SQLITE_ERROR";
    case SQLITE_PERM:       return "SQLITE_PERM";
    case SQLITE_ABORT:      return "SQLITE_ABORT";
    case SQLITE_BUSY:       return "SQLITE_BUSY";
    case SQLITE_NOMEM:      return "SQLITE_NOMEM";
    case SQLITE_READONLY:   return "SQLITE_READONLY";
    case SQLITE_INTERRUPT:  return "SQLITE_INTERRUPT";
    case SQLITE_IOERR:      return "SQLITE_IOERR";
    case SQLITE_CORRUPT:    return "SQLITE_CORRUPT";
    case SQLITE_FULL:       return "SQLITE_FULL";
    case SQLITE_CANTOPEN:   return "SQLITE_CANTOPEN";
    case SQLITE_PROTOCOL:   return "SQLITE_PROTOCOL";
    case SQLITE_EMPTY:      return "SQLITE_EMPTY";
    case SQLITE_SCHEMA:     return "SQLITE_SCHEMA";
    case SQLITE_CONSTRAINT: return "SQLITE_CONSTRAINT";
    case SQLITE_MISMATCH:   return "SQLITE_MISMATCH";
    case SQLITE_MISUSE:     return "SQLITE_MISUSE";
    case SQLITE_NOLFS:      return "SQLITE_NOLFS";
    case SQLITE_FORMAT:     return "SQLITE_FORMAT";
    case SQLITE_RANGE:      return "SQLITE_RANGE";
    case SQLITE_NOTADB:     return "SQLITE_NOTADB";
    default: {
      sqlite3_snprintf(sizeof(zCode),zCode,"error code %d",iCode);
    }
  }
  return zCode;
}

/* Error logs from SQLite */
void geomvcs_sqlite_log(struct vcs_db *db, void *notUsed, int iCode, const char *zErrmsg){
  geomvcs_warning(db, "%s: %s", sqlite_error_code_name(iCode), zErrmsg);
}

/*
** Print a list of words in multiple columns.
*/
static void multi_column_list(const char **azWord, int nWord){
  int i, j, len;
  int mxLen = 0;
  int nCol;
  int nRow;
  for(i=0; i<nWord; i++){
    len = strlen(azWord[i]);
    if( len>mxLen ) mxLen = len;
  }
  nCol = 80/(mxLen+2);
  if( nCol==0 ) nCol = 1;
  nRow = (nWord + nCol - 1)/nCol;
  for(i=0; i<nRow; i++){
    const char *zSpacer = "";
    for(j=i; j<nWord; j+=nRow){
      printf("%s%-*s", zSpacer, mxLen, azWord[j]);
      zSpacer = "  ";
    }
    printf("\n");
  }
}

/*
 * ** We find that the built-in isspace() function does not work for
 * ** some international character sets.  So here is a substitute.
 * */
int geomvcs_isspace(char c){
	  return c==' ' || (c<='\r' && c>='\t');
}

/*
 * ** Other replacements for ctype.h functions.
 * */
int geomvcs_islower(char c){ return c>='a' && c<='z'; }
int geomvcs_isupper(char c){ return c>='A' && c<='Z'; }
int geomvcs_isdigit(char c){ return c>='0' && c<='9'; }
int geomvcs_tolower(char c){
	  return geomvcs_isupper(c) ? c - 'A' + 'a' : c;
}
int geomvcs_isalpha(char c){
	  return (c>='a' && c<='z') || (c>='A' && c<='Z');
}
int geomvcs_isalnum(char c){
	  return (c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9');
}

