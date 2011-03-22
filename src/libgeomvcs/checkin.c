/*
** Copyright (c) 2007 D. Richard Hipp
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
** This file contains code used to check-in versions of the project
** from the local repository.
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/basic.h"
#include "geomvcs/blob.h"
#include "geomvcs/schema.h"
#include "geomvcs/printf.h"
#include "geomvcs/manifest.h"
#include "geomvcs/md5.h"
#include "geomvcs/sync.h"
#include "geomvcs/file.h"
#include "geomvcs/content.h"
#include "geomvcs/db.h"
#include <assert.h>

/*
** Generate text describing all changes.  Prepend zPrefix to each line
** of output.
**
** We assume that vfile_check_signature has been run.
**
** If missingIsFatal is true, then any files that are missing or which
** are not true files results in a fatal error.
*/
static void status_report( 
  struct vcs_db *db,
  Blob *report,          /* Append the status report here */
  const char *zPrefix,   /* Prefix on each line of the report */
  int missingIsFatal     /* MISSING and NOT_A_FILE are fatal errors */
){
  Stmt q;
  int nPrefix = strlen(zPrefix);
  int nErr = 0;
  db_prepare(db, &q, 
    "SELECT pathname, deleted, chnged, rid, coalesce(origname!=pathname,0)"
    "  FROM vfile "
    " WHERE file_is_selected(id)"
    "   AND (chnged OR deleted OR rid=0 OR pathname!=origname) ORDER BY 1"
  );
  while( db_step(&q)==SQLITE_ROW ){
    const char *zPathname = db_column_text(&q,0);
    int isDeleted = db_column_int(&q, 1);
    int isChnged = db_column_int(&q,2);
    int isNew = db_column_int(&q,3)==0;
    int isRenamed = db_column_int(&q,4);
    char *zFullName = mprintf(db, "%s%s", db->zLocalRoot, zPathname);
    blob_append(db, report, zPrefix, nPrefix);
    if( isDeleted ){
      blob_appendf(db, report, "DELETED    %s\n", zPathname);
    }else if( !file_isfile(zFullName) ){
      if( access(zFullName, 0)==0 ){
        blob_appendf(db, report, "NOT_A_FILE %s\n", zPathname);
        if( missingIsFatal ){
          geomvcs_warning(db, "not a file: %s", zPathname);
          nErr++;
        }
      }else{
        blob_appendf(db, report, "MISSING    %s\n", zPathname);
        if( missingIsFatal ){
          geomvcs_warning(db, "missing file: %s", zPathname);
          nErr++;
        }
      }
    }else if( isNew ){
      blob_appendf(db, report, "ADDED      %s\n", zPathname);
    }else if( isDeleted ){
      blob_appendf(db, report, "DELETED    %s\n", zPathname);
    }else if( isChnged==2 ){
      blob_appendf(db, report, "UPDATED_BY_MERGE %s\n", zPathname);
    }else if( isChnged==3 ){
      blob_appendf(db, report, "ADDED_BY_MERGE %s\n", zPathname);
    }else if( isChnged==1 ){
      blob_appendf(db, report, "EDITED     %s\n", zPathname);
    }else if( isRenamed ){
      blob_appendf(db, report, "RENAMED    %s\n", zPathname);
    }
    free(zFullName);
  }
  db_finalize(db, &q);
  db_prepare(db, &q, "SELECT uuid FROM vmerge JOIN blob ON merge=rid"
                 " WHERE id=0");
  while( db_step(&q)==SQLITE_ROW ){
    blob_append(db, report, zPrefix, nPrefix);
    blob_appendf(db, report, "MERGED_WITH %s\n", db_column_text(&q, 0));
  }
  db_finalize(db, &q);
  if( nErr ){
    geomvcs_fatal(db, "aborting due to prior errors");
  }
}

/*
** Construct and return a string which is an SQL expression that will
** be TRUE if value zVal matches any of the GLOB expressions in the list
** zGlobList.  For example:
**
**    zVal:       "x"
**    zGlobList:  "*.o,*.obj"
**
**    Result:     "(x GLOB '*.o' OR x GLOB '*.obj')"
**
** Each element of the GLOB list may optionally be enclosed in either '...'
** or "...".  This allows commas in the expression.  Whitespace at the
** beginning and end of each GLOB pattern is ignored, except when enclosed
** within '...' or "...".
**
** This routine makes no effort to free the memory space it uses.
*/
char *glob_expr(struct vcs_db *db, const char *zVal, const char *zGlobList){
  Blob expr;
  char *zSep = "(";
  int nTerm = 0;
  int i;
  int cTerm;

  if( zGlobList==0 || zGlobList[0]==0 ) return "0";
  blob_zero(&expr);
  while( zGlobList[0] ){
    while( geomvcs_isspace(zGlobList[0]) || zGlobList[0]==',' ) zGlobList++;
    if( zGlobList[0]==0 ) break;
    if( zGlobList[0]=='\'' || zGlobList[0]=='"' ){
      cTerm = zGlobList[0];
      zGlobList++;
    }else{
      cTerm = ',';
    }
    for(i=0; zGlobList[i] && zGlobList[i]!=cTerm; i++){}
    if( cTerm==',' ){
      while( i>0 && geomvcs_isspace(zGlobList[i-1]) ){ i--; }
    }
    blob_appendf(db, &expr, "%s%s GLOB '%.*q'", zSep, zVal, i, zGlobList);
    zSep = " OR ";
    if( cTerm!=',' && zGlobList[i] ) i++;
    zGlobList += i;
    if( zGlobList[0] ) zGlobList++;
    nTerm++;
  }
  if( nTerm ){
    blob_appendf(db, &expr, ")");
    return blob_str(db, &expr);
  }else{
    return "0";
  }
}

/*
** Prepare a commit comment.  Let the user modify it using the
** editor specified in the global_config table or either
** the VISUAL or EDITOR environment variable.
**
** Store the final commit comment in pComment.  pComment is assumed
** to be uninitialized - any prior content is overwritten.
**
** zInit is the text of the most recent failed attempt to check in
** this same change.  Use zInit to reinitialize the check-in comment
** so that the user does not have to retype.
**
** zBranch is the name of a new branch that this check-in is forced into.
** zBranch might be NULL or an empty string if no forcing occurs.
**
** parent_rid is the recordid of the parent check-in.
*/
static void prepare_commit_comment(
  struct vcs_db *db, 
  Blob *pComment,
  char *zInit,
  const char *zBranch,
  int parent_rid
){
  const char *zEditor;
  char *zCmd;
  char *zFile;
  Blob text, line;
  char *zComment;
  int i;
  blob_init(&text, zInit, -1);
  blob_append(db, &text,
    "\n"
    "# Enter comments on this check-in.  Lines beginning with # are ignored.\n"
    "# The check-in comment follows wiki formatting rules.\n"
    "#\n", -1
  );
  blob_appendf(db, &text, "# user: %s\n", db->zLogin);
  if( zBranch && zBranch[0] ){
    blob_appendf(db, &text, "# tags: %s\n#\n", zBranch);
  }else{
    char *zTags = info_tags_of_checkin(parent_rid, 1);
    if( zTags )  blob_appendf(db, &text, "# tags: %z\n#\n", zTags);
  }
  if( db->markPrivate ){
    blob_append(db, &text,
      "# PRIVATE BRANCH: This check-in will be private and will not sync to\n"
      "# repositories.\n"
      "#\n", -1
    );
  }
  status_report(db, &text, "# ", 1);
  zEditor = db_get(db, "editor", 0);
  if( zEditor==0 ){
    zEditor = getenv("VISUAL");
  }
  if( zEditor==0 ){
    zEditor = getenv("EDITOR");
  }
  if( zEditor==0 ){
    blob_append(db, &text,
       "#\n"
       "# Since no default text editor is set using EDITOR or VISUAL\n"
       "# environment variables or the \"fossil set editor\" command,\n"
       "# and because no check-in comment was specified using the \"-m\"\n"
       "# or \"-M\" command-line options, you will need to enter the\n"
       "# check-in comment below.  Type \".\" on a line by itself when\n"
       "# you are done:\n", -1);
    zFile = mprintf(db, "-");
  }else{
    zFile = db_text(db, 0, "SELECT '%qci-comment-' || hex(randomblob(6)) || '.txt'",
                    db->zLocalRoot);
  }
#if defined(_WIN32)
  blob_add_cr(&text);
#endif
  blob_write_to_file(db, &text, zFile);
  if( zEditor ){
    zCmd = mprintf(db, "%s \"%s\"", zEditor, zFile);
    printf("%s\n", zCmd);
    if( fossil_system(zCmd) ){
      geomvcs_panic(db, "editor aborted");
    }
    blob_reset(db, &text);
    blob_read_from_file(db, &text, zFile);
  }else{
    char zIn[300];
    blob_reset(db, &text);
    while( fgets(zIn, sizeof(zIn), stdin)!=0 ){
      if( zIn[0]=='.' && (zIn[1]==0 || zIn[1]=='\r' || zIn[1]=='\n') ) break;
      blob_append(db, &text, zIn, -1);
    }
  }
  blob_remove_cr(db, &text);
  unlink(zFile);
  free(zFile);
  blob_zero(pComment);
  while( blob_line(&text, &line) ){
    int i, n;
    char *z;
    n = blob_size(&line);
    z = blob_buffer(&line);
    for(i=0; i<n && geomvcs_isspace(z[i]);  i++){}
    if( i<n && z[i]=='#' ) continue;
    if( i<n || blob_size(pComment)>0 ){
      blob_appendf(db, pComment, "%b", &line);
    }
  }
  blob_reset(db, &text);
  zComment = blob_str(db, pComment);
  i = strlen(zComment);
  while( i>0 && geomvcs_isspace(zComment[i-1]) ){ i--; }
  blob_resize(db, pComment, i);
}

/*
** Return true if the check-in with RID=rid is a leaf.
** A leaf has no children in the same branch. 
*/
int is_a_leaf(struct vcs_db *db, int rid){
  int rc;
  static const char zSql[] = 
    "SELECT 1 FROM plink\n"
    " WHERE pid=%d\n"
    "   AND coalesce((SELECT value FROM tagxref\n"
    "                  WHERE tagid=%d AND rid=plink.pid), 'trunk')\n"
    "      =coalesce((SELECT value FROM tagxref\n"
    "                  WHERE tagid=%d AND rid=plink.cid), 'trunk')\n"
  ;
  rc = db_int(db, 0, zSql, rid, TAG_BRANCH, TAG_BRANCH);
  return rc==0;
}

/*
** Make sure the current check-in with timestamp zDate is younger than its
** ancestor identified rid and zUuid.  Throw a fatal error if not.
*/
static void checkin_verify_younger(
  struct vcs_db *db, 
  int rid,              /* The record ID of the ancestor */
  const char *zUuid,    /* The artifact ID of the ancestor */
  const char *zDate     /* Date & time of the current check-in */
){
#ifndef FOSSIL_ALLOW_OUT_OF_ORDER_DATES
  int b;
  b = db_exists(db, 
    "SELECT 1 FROM event"
    " WHERE datetime(mtime)>=%Q"
    "   AND type='ci' AND objid=%d",
    zDate, rid
  );
  if( b ){
    geomvcs_fatal(db, "ancestor check-in [%.10s] (%s) is not older (clock skew?)"
                 " Use -f to override.", zUuid, zDate);
  }
#endif
}

/*
** zDate should be a valid date string.  Convert this string into the
** format YYYY-MM-DDTHH:MM:SS.  If the string is not a valid date, 
** print a fatal error and quit.
*/
char *date_in_standard_format(struct vcs_db *db, const char *zInputDate){
  char *zDate;
  zDate = db_text(db, 0, "SELECT strftime('%%Y-%%m-%%dT%%H:%%M:%%f',%Q)",
                  zInputDate);
  if( zDate[0]==0 ){
    geomvcs_fatal(db, 
      "unrecognized date format (%s): use \"YYYY-MM-DD HH:MM:SS.SSS\"",
      zInputDate
    );
  }
  return zDate;
}

/*
** Create a manifest.
*/
static void create_manifest(
  struct vcs_db *db,
  Blob *pOut,                 /* Write the manifest here */
  const char *zBaselineUuid,  /* UUID of baseline, or zero */
  Manifest *pBaseline,        /* Make it a delta manifest if not zero */
  Blob *pComment,             /* Check-in comment text */
  int vid,                    /* blob-id of the parent manifest */
  int verifyDate,             /* Verify that child is younger */
  Blob *pCksum,               /* Repository checksum.  May be 0 */
  const char *zDateOvrd,      /* Date override.  If 0 then use 'now' */
  const char *zUserOvrd,      /* User override.  If 0 then use db->zLogin */
  const char *zBranch,        /* Branch name.  May be 0 */
  const char *zBgColor,       /* Background color.  May be 0 */
  const char *zTag,           /* Tag to apply to this check-in */
  int *pnFBcard               /* Number of generated B- and F-cards */
){
  char *zDate;                /* Date of the check-in */
  char *zParentUuid;          /* UUID of parent check-in */
  Blob filename;              /* A single filename */
  int nBasename;              /* Size of base filename */
  Stmt q;                     /* Query of files changed */
  Stmt q2;                    /* Query of merge parents */
  Blob mcksum;                /* Manifest checksum */
  ManifestFile *pFile;        /* File from the baseline */
  int nFBcard = 0;            /* Number of B-cards and F-cards */

  assert( pBaseline==0 || pBaseline->zBaseline==0 );
  assert( pBaseline==0 || zBaselineUuid!=0 );
  blob_zero(pOut);
  zParentUuid = db_text(db, 0, "SELECT uuid FROM blob WHERE rid=%d", vid);
  if( pBaseline ){
    blob_appendf(db, pOut, "B %s\n", zBaselineUuid);
    manifest_file_rewind(db, pBaseline);
    pFile = manifest_file_next(pBaseline, 0);
    nFBcard++;
  }else{
    pFile = 0;
  }
  blob_appendf(db, pOut, "C %F\n", blob_str(db, pComment));
  zDate = date_in_standard_format(db, zDateOvrd ? zDateOvrd : "now");
  blob_appendf(db, pOut, "D %s\n", zDate);
  zDate[10] = ' ';
  db_prepare(db, &q,
    "SELECT pathname, uuid, origname, blob.rid, isexe,"
    "       file_is_selected(vfile.id)"
    "  FROM vfile JOIN blob ON vfile.mrid=blob.rid"
    " WHERE (NOT deleted OR NOT file_is_selected(vfile.id))"
    "   AND vfile.vid=%d"
    " ORDER BY 1", vid);
  blob_zero(&filename);
  blob_appendf(db, &filename, "%s", db->zLocalRoot);
  nBasename = blob_size(&filename);
  while( db_step(&q)==SQLITE_ROW ){
    const char *zName = db_column_text(&q, 0);
    const char *zUuid = db_column_text(&q, 1);
    const char *zOrig = db_column_text(&q, 2);
    int frid = db_column_int(&q, 3);
    int isexe = db_column_int(&q, 4);
    int isSelected = db_column_int(&q, 5);
    const char *zPerm;
    int cmp;
#if !defined(_WIN32)
    /* For unix, extract the "executable" permission bit directly from
    ** the filesystem.  On windows, the "executable" bit is retained
    ** unchanged from the original. 
    */
    blob_resize(db, &filename, nBasename);
    blob_append(db, &filename, zName, -1);
    isexe = file_isexe(blob_str(db, &filename));
#endif
    if( isexe ){
      zPerm = " x";
    }else{
      zPerm = "";
    }
    if( !db->markPrivate ) content_make_public(db, frid);
    while( pFile && geomvcs_strcmp(pFile->zName,zName)<0 ){
      blob_appendf(db, pOut, "F %F\n", pFile->zName);
      pFile = manifest_file_next(pBaseline, 0);
      nFBcard++;
    }
    cmp = 1;
    if( pFile==0
      || (cmp = geomvcs_strcmp(pFile->zName,zName))!=0
      || geomvcs_strcmp(pFile->zUuid, zUuid)!=0
    ){
      if( zOrig && !isSelected ){ zName = zOrig; zOrig = 0; }
      if( zOrig==0 || geomvcs_strcmp(zOrig,zName)==0 ){
        blob_appendf(db, pOut, "F %F %s%s\n", zName, zUuid, zPerm);
      }else{
        if( zPerm[0]==0 ){ zPerm = " w"; }
        blob_appendf(db, pOut, "F %F %s%s %F\n", zName, zUuid, zPerm, zOrig);
      }
      nFBcard++;
    }
    if( cmp==0 ) pFile = manifest_file_next(pBaseline,0);
  }
  blob_reset(db, &filename);
  db_finalize(db, &q);
  while( pFile ){
    blob_appendf(db, pOut, "F %F\n", pFile->zName);
    pFile = manifest_file_next(pBaseline, 0);
    nFBcard++;
  }
  blob_appendf(db, pOut, "P %s", zParentUuid);
  if( verifyDate ) checkin_verify_younger(db, vid, zParentUuid, zDate);
  free(zParentUuid);
  db_prepare(db, &q2, "SELECT merge FROM vmerge WHERE id=:id");
  db_bind_int(db, &q2, ":id", 0);
  while( db_step(&q2)==SQLITE_ROW ){
    char *zMergeUuid;
    int mid = db_column_int(&q2, 0);
    if( !db->markPrivate && content_is_private(db, mid) ) continue;
    zMergeUuid = db_text(db, 0, "SELECT uuid FROM blob WHERE rid=%d", mid);
    if( zMergeUuid ){
      blob_appendf(db, pOut, " %s", zMergeUuid);
      if( verifyDate ) checkin_verify_younger(db, mid, zMergeUuid, zDate);
      free(zMergeUuid);
    }
  }
  db_finalize(db, &q2);
  free(zDate);

  blob_appendf(db, pOut, "\n");
  if( pCksum ) blob_appendf(db, pOut, "R %b\n", pCksum);
  if( zBranch && zBranch[0] ){
    /* Set tags for the new branch */
    if( zBgColor && zBgColor[0] ){
      blob_appendf(db, pOut, "T *bgcolor * %F\n", zBgColor);
    }
    blob_appendf(db, pOut, "T *branch * %F\n", zBranch);
    blob_appendf(db, pOut, "T *sym-%F *\n", zBranch);
  }
  if( db->markPrivate ){
    /* If this manifest is private, mark it as such */
    blob_appendf(db, pOut, "T +private *\n");
  }
  if( zTag && zTag[0] ){
    /* Add a symbolic tag to this check-in */
    blob_appendf(db, pOut, "T +sym-%F *\n", zTag);
  }
  if( zBranch && zBranch[0] ){
    /* For a new branch, cancel all prior propagating tags */
    Stmt q;
    db_prepare(db, &q,
        "SELECT tagname FROM tagxref, tag"
        " WHERE tagxref.rid=%d AND tagxref.tagid=tadb->tagid"
        "   AND tagtype==2 AND tagname GLOB 'sym-*'"
        "   AND tagname!='sym-'||%Q"
        " ORDER BY tagname",
        vid, zBranch);
    while( db_step(&q)==SQLITE_ROW ){
      const char *zTag = db_column_text(&q, 0);
      blob_appendf(db, pOut, "T -%F *\n", zTag);
    }
    db_finalize(db, &q);
  }  
  blob_appendf(db, pOut, "U %F\n", zUserOvrd ? zUserOvrd : db->zLogin);
  md5sum_blob(db, pOut, &mcksum);
  blob_appendf(db, pOut, "Z %b\n", &mcksum);
  if( pnFBcard ) *pnFBcard = nFBcard;
}

/*
** Issue a warning and give the user an opportunity to abandon out
** if a \r\n line ending is seen in a text file.
*/
static void cr_warning(struct vcs_db *db, const Blob *p, const char *zFilename){
  int nCrNl = 0;          /* Number of \r\n line endings seen */
  const unsigned char *z; /* File text */
  int n;                  /* Size of the file in bytes */
  int lastNl = 0;         /* Characters since last \n */
  int i;                  /* Loop counter */
  char *zMsg;             /* Warning message */
  Blob fname;             /* Relative pathname of the file */
  Blob ans;               /* Answer to continue prompt */
  static int allOk = 0;   /* Set to true to disable this routine */

  if( allOk ) return;
  z = (unsigned char*)blob_buffer(p);
  n = blob_size(p);
  for(i=0; i<n-1; i++){
    unsigned char c = z[i];
    if( c==0 ) return;   /* It's binary */
    if( c=='\n' ){
      if( i>0 && z[i-1]=='\r' ){
        nCrNl++;
        if( i>10000 ) break;
      }
      lastNl = 0;
    }else{
      lastNl++;
      if( lastNl>1000 ) return;   /* Binary if any line longer than 1000 */
    }
  }
  if( nCrNl ){
    char c;
    file_relative_name(db, zFilename, &fname);
    blob_zero(&ans);
    zMsg = mprintf(db, "%s contains CR/NL line endings; commit anyhow (y/N/a)?", 
                   blob_str(db, &fname));
    prompt_user(zMsg, &ans);
    geomvcs_free(zMsg);
    c = blob_str(db, &ans)[0];
    if( c=='a' ){
      allOk = 1;
    }else if( c!='y' ){
      geomvcs_fatal(db, "Abandoning commit due to CR+NL line endings in %s",
                   blob_str(db, &fname));
    }
    blob_reset(db, &ans);
    blob_reset(db, &fname);
  }
}
