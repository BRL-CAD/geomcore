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
** COMMAND: changes
**
** Usage: %fossil changes
**
** Report on the edit status of all files in the current checkout.
** See also the "status" and "extra" commands.
**
** Options:
**
**    --sha1sum         Verify file status using SHA1 hashing rather
**                      than relying on file mtimes.
*/
void changes_cmd(struct vcs_db *db){
  Blob report;
  int vid;
  int useSha1sum = find_option(db, "sha1sum", 0, 0)!=0;
  db_must_be_within_tree(db);
  blob_zero(&report);
  vid = db_lget_int("checkout", 0);
  vfile_check_signature(vid, 0, useSha1sum);
  status_report(db, &report, "", 0);
  blob_write_to_file(&report, "-");
}

/*
** COMMAND: status
**
** Usage: %fossil status
**
** Report on the status of the current checkout.
**
** Options:
**
**    --sha1sum         Verify file status using SHA1 hashing rather
**                      than relying on file mtimes.
*/
void status_cmd(struct vcs_db *db){
  int vid;
  db_must_be_within_tree(db);
       /* 012345678901234 */
  printf("repository:   %s\n", db_lget(db, "repository",""));
  printf("local-root:   %s\n", db->zLocalRoot);
  printf("server-code:  %s\n", db_get(db, "server-code", ""));
  vid = db_lget_int("checkout", 0);
  if( vid ){
    show_common_info(vid, "checkout:", 1, 1);
  }
  changes_cmd(db);
}

/*
** COMMAND: ls
**
** Usage: %fossil ls [-l]
**
** Show the names of all files in the current checkout.  The -l provides
** extra information about each file.
*/
void ls_cmd(struct vcs_db *db){
  int vid;
  Stmt q;
  int isBrief;

  isBrief = find_option(db, "l","l", 0)==0;
  db_must_be_within_tree(db);
  vid = db_lget_int("checkout", 0);
  vfile_check_signature(vid, 0, 0);
  db_prepare(db, &q,
     "SELECT pathname, deleted, rid, chnged, coalesce(origname!=pathname,0)"
     "  FROM vfile"
     " ORDER BY 1"
  );
  while( db_step(&q)==SQLITE_ROW ){
    const char *zPathname = db_column_text(&q,0);
    int isDeleted = db_column_int(&q, 1);
    int isNew = db_column_int(&q,2)==0;
    int chnged = db_column_int(&q,3);
    int renamed = db_column_int(&q,4);
    char *zFullName = mprintf(db, "%s%s", db->zLocalRoot, zPathname);
    if( isBrief ){
      printf("%s\n", zPathname);
    }else if( isNew ){
      printf("ADDED      %s\n", zPathname);
    }else if( isDeleted ){
      printf("DELETED    %s\n", zPathname);
    }else if( !file_isfile(zFullName) ){
      if( access(zFullName, 0)==0 ){
        printf("NOT_A_FILE %s\n", zPathname);
      }else{
        printf("MISSING    %s\n", zPathname);
      }
    }else if( chnged ){
      printf("EDITED     %s\n", zPathname);
    }else if( renamed ){
      printf("RENAMED    %s\n", zPathname);
    }else{
      printf("UNCHANGED  %s\n", zPathname);
    }
    free(zFullName);
  }
  db_finalize(db, &q);
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
** COMMAND: extras
** Usage: %fossil extras ?--dotfiles? ?--ignore GLOBPATTERN?
**
** Print a list of all files in the source tree that are not part of
** the current checkout.  See also the "clean" command.
**
** Files and subdirectories whose names begin with "." are normally
** ignored but can be included by adding the --dotfiles option.
**
** The GLOBPATTERN is a comma-separated list of GLOB expressions for
** files that are ignored.  The GLOBPATTERN specified by the "ignore-glob"
** is used if the --ignore option is omitted.
*/
void extra_cmd(struct vcs_db *db){
  Blob path;
  Blob repo;
  Stmt q;
  int n;
  const char *zIgnoreFlag = find_option(db, "ignore",0,1);
  int allFlag = find_option(db, "dotfiles",0,0)!=0;
  int outputManifest;

  db_must_be_within_tree(db);
  outputManifest = db_get_boolean("manifest",0);
  db_multi_exec(db, "CREATE TEMP TABLE sfile(x TEXT PRIMARY KEY)");
  n = strlen(db->zLocalRoot);
  blob_init(&path, db->zLocalRoot, n-1);
  if( zIgnoreFlag==0 ){
    zIgnoreFlag = db_get(db, "ignore-glob", 0);
  }
  vfile_scan(0, &path, blob_size(&path), allFlag);
  db_prepare(db, &q, 
      "SELECT x FROM sfile"
      " WHERE x NOT IN (%s)"
      "   AND NOT %s"
      " ORDER BY 1",
      fossil_all_reserved_names(),
      glob_expr(db, "x", zIgnoreFlag)
  );
  if( file_tree_name(db->zRepositoryName, &repo, 0) ){
    db_multi_exec(db, "DELETE FROM sfile WHERE x=%B", &repo);
  }
  while( db_step(&q)==SQLITE_ROW ){
    printf("%s\n", db_column_text(&q, 0));
  }
  db_finalize(db, &q);
}

/*
** COMMAND: clean
** Usage: %fossil clean ?--force? ?--dotfiles? ?--ignore GLOBPATTERN?
**
** Delete all "extra" files in the source tree.  "Extra" files are
** files that are not officially part of the checkout.  See also
** the "extra" command. This operation cannot be undone. 
**
** You will be prompted before removing each file. If you are
** sure you wish to remove all "extra" files you can specify the
** optional --force flag and no prompts will be issued.
**
** Files and subdirectories whose names begin with "." are
** normally ignored.  They are included if the "--dotfiles" option
** is used.
**
** The GLOBPATTERN is a comma-separated list of GLOB expressions for
** files that are ignored.  The GLOBPATTERN specified by the "ignore-glob"
** is used if the --ignore option is omitted.
*/
void clean_cmd(struct vcs_db *db){
  int allFlag;
  int dotfilesFlag;
  const char *zIgnoreFlag;
  Blob path, repo;
  Stmt q;
  int n;
  allFlag = find_option(db, "force","f",0)!=0;
  dotfilesFlag = find_option(db, "dotfiles",0,0)!=0;
  zIgnoreFlag = find_option(db, "ignore",0,1);
  db_must_be_within_tree(db);
  if( zIgnoreFlag==0 ){
    zIgnoreFlag = db_get(db, "ignore-glob", 0);
  }
  db_multi_exec(db, "CREATE TEMP TABLE sfile(x TEXT PRIMARY KEY)");
  n = strlen(db->zLocalRoot);
  blob_init(&path, db->zLocalRoot, n-1);
  vfile_scan(0, &path, blob_size(&path), dotfilesFlag);
  db_prepare(db, &q, 
      "SELECT %Q || x FROM sfile"
      " WHERE x NOT IN (%s) AND NOT %s"
      " ORDER BY 1",
      db->zLocalRoot, fossil_all_reserved_names(), glob_expr(db, "x",zIgnoreFlag)
  );
  if( file_tree_name(db->zRepositoryName, &repo, 0) ){
    db_multi_exec(db, "DELETE FROM sfile WHERE x=%B", &repo);
  }
  while( db_step(&q)==SQLITE_ROW ){
    if( allFlag ){
      unlink(db_column_text(&q, 0));
    }else{
      Blob ans;
      char *prompt = mprintf(db, "remove unmanaged file \"%s\" (y/N)? ",
                              db_column_text(&q, 0));
      blob_zero(&ans);
      prompt_user(prompt, &ans);
      if( blob_str(db, &ans)[0]=='y' ){
        unlink(db_column_text(&q, 0));
      }
    }
  }
  db_finalize(db, &q);
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
  blob_write_to_file(&text, zFile);
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
  blob_remove_cr(&text);
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
** Populate the Global.aCommitFile[] based on the command line arguments
** to a [commit] command. Global.aCommitFile is an array of integers
** sized at (N+1), where N is the number of arguments passed to [commit].
** The contents are the [id] values from the vfile table corresponding
** to the filenames passed as arguments.
**
** The last element of aCommitFile[] is always 0 - indicating the end
** of the array.
**
** If there were no arguments passed to [commit], aCommitFile is not
** allocated and remains NULL. Other parts of the code interpret this
** to mean "all files".
*/
void select_commit_files(struct vcs_db *db){
  if( db->argc>2 ){
    int ii;
    Blob b;
    blob_zero(&b);
    db->aCommitFile = geomvcs_malloc(db, sizeof(int)*(db->argc-1));

    for(ii=2; ii<db->argc; ii++){
      int iId;
      file_tree_name(db->argv[ii], &b, 1);
      iId = db_int(db, -1, "SELECT id FROM vfile WHERE pathname=%Q", blob_str(db, &b));
      if( iId<0 ){
        geomvcs_fatal(db, "fossil knows nothing about: %s", db->argv[ii]);
      }
      db->aCommitFile[ii-2] = iId;
      blob_reset(db, &b);
    }
    db->aCommitFile[ii-2] = 0;
  }
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
    manifest_file_rewind(pBaseline);
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
    if( !db->markPrivate ) content_make_public(frid);
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
    if( !db->markPrivate && content_is_private(mid) ) continue;
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
    file_relative_name(zFilename, &fname);
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

/*
** COMMAND: ci
** COMMAND: commit
**
** Usage: %fossil commit ?OPTIONS? ?FILE...?
**
** Create a new version containing all of the changes in the current
** checkout.  You will be prompted to enter a check-in comment unless
** the comment has been specified on the command-line using "-m" or a 
** file containing the comment using -M.  The editor defined in the
** "editor" fossil option (see %fossil help set) will be used, or from
** the "VISUAL" or "EDITOR" environment variables (in that order) if
** no editor is set.
**
** All files that have changed will be committed unless some subset of
** files is specified on the command line.
**
** The --branch option followed by a branch name causes the new check-in
** to be placed in the named branch.  The --bgcolor option can be followed
** by a color name (ex:  '#ffc0c0') to specify the background color of
** entries in the new branch when shown in the web timeline interface.
**
** A check-in is not permitted to fork unless the --force or -f
** option appears.  A check-in is not allowed against a closed check-in.
**
** The --private option creates a private check-in that is never synced.
** Children of private check-ins are automatically private.
**
** the --tag option applies the symbolic tag name to the check-in.
**
** Options:
**
**    --comment|-m COMMENT-TEXT
**    --message-file|-M COMMENT-FILE
**    --branch NEW-BRANCH-NAME
**    --bgcolor COLOR
**    --nosign
**    --force|-f
**    --private
**    --baseline
**    --delta
**    --tag TAG-NAME
**    
*/
void commit_cmd(struct vcs_db *db){
  int hasChanges;        /* True if unsaved changes exist */
  int vid;               /* blob-id of parent version */
  int nrid;              /* blob-id of a modified file */
  int nvid;              /* Blob-id of the new check-in */
  Blob comment;          /* Check-in comment */
  const char *zComment;  /* Check-in comment */
  Stmt q;                /* Query to find files that have been modified */
  char *zUuid;           /* UUID of the new check-in */
  int noSign = 0;        /* True to omit signing the manifest using GPG */
  int isAMerge = 0;      /* True if checking in a merge */
  int forceFlag = 0;     /* Force a fork */
  int forceDelta = 0;    /* Force a delta-manifest */
  int forceBaseline = 0; /* Force a baseline-manifest */
  char *zManifestFile;   /* Name of the manifest file */
  int useCksum;          /* True if checksums should be computed and verified */
  int outputManifest;    /* True to output "manifest" and "manifest.uuid" */
  int testRun;           /* True for a test run.  Debugging only */
  const char *zBranch;   /* Create a new branch with this name */
  const char *zBgColor;  /* Set background color when branching */
  const char *zDateOvrd; /* Override date string */
  const char *zUserOvrd; /* Override user name */
  const char *zComFile;  /* Read commit message from this file */
  const char *zTag;      /* Symbolic tag to apply to this check-in */
  Blob manifest;         /* Manifest in baseline form */
  Blob muuid;            /* Manifest uuid */
  Blob cksum1, cksum2;   /* Before and after commit checksums */
  Blob cksum1b;          /* Checksum recorded in the manifest */
  int szD;               /* Size of the delta manifest */
  int szB;               /* Size of the baseline manifest */
 
  url_proxy_options();
  noSign = find_option(db, "nosign",0,0)!=0;
  forceDelta = find_option(db, "delta",0,0)!=0;
  forceBaseline = find_option(db, "baseline",0,0)!=0;
  if( forceDelta && forceBaseline ){
    geomvcs_fatal(db, "cannot use --delta and --baseline together");
  }
  testRun = find_option(db, "test",0,0)!=0;
  zComment = find_option(db, "comment","m",1);
  forceFlag = find_option(db, "force", "f", 0)!=0;
  zBranch = find_option(db, "branch","b",1);
  zBgColor = find_option(db, "bgcolor",0,1);
  zTag = find_option(db, "tag",0,1);
  zComFile = find_option(db, "message-file", "M", 1);
  if( find_option(db, "private",0,0) ){
    db->markPrivate = 1;
    if( zBranch==0 ) zBranch = "private";
    if( zBgColor==0 ) zBgColor = "#fec084";  /* Orange */
  }
  zDateOvrd = find_option(db, "date-override",0,1);
  zUserOvrd = find_option(db, "user-override",0,1);
  db_must_be_within_tree(db);
  noSign = db_get_boolean("omitsign", 0)|noSign;
  if( db_get_boolean("clearsign", 0)==0 ){ noSign = 1; }
  useCksum = db_get_boolean("repo-cksum", 1);
  outputManifest = db_get_boolean("manifest", 0);
  verify_all_options();

  /* So that older versions of Fossil (that do not understand delta-
  ** manifest) can continue to use this repository, do not create a new
  ** delta-manifest unless this repository already contains one or more
  ** delta-manifets, or unless the delta-manifest is explicitly requested
  ** by the --delta option.
  */
  if( !forceDelta && !db_get_boolean("seen-delta-manifest",0) ){
    forceBaseline = 1;
  }

  /* Get the ID of the parent manifest artifact */
  vid = db_lget_int("checkout", 0);
  if( content_is_private(vid) ){
    db->markPrivate = 1;
  }

  /*
  ** Autosync if autosync is enabled and this is not a private check-in.
  */
  if( !db->markPrivate ){
    if( autosync(AUTOSYNC_PULL) ){
      Blob ans;
      blob_zero(&ans);
      prompt_user("continue in spite of sync failure (y/N)? ", &ans);
      if( blob_str(db, &ans)[0]!='y' ){
        geomvcs_exit(db, 1);
      }
    }
  }

  /* Require confirmation to continue with the check-in if there is
  ** clock skew
  */
  if( db->clockSkewSeen ){
    Blob ans;
    blob_zero(&ans);
    prompt_user("continue in spite of time skew (y/N)? ", &ans);
    if( blob_str(db, &ans)[0]!='y' ){
      geomvcs_exit(db, 1);
    }
  }

  /* There are two ways this command may be executed. If there are
  ** no arguments following the word "commit", then all modified files
  ** in the checked out directory are committed. If one or more arguments
  ** follows "commit", then only those files are committed.
  **
  ** After the following function call has returned, the Global.aCommitFile[]
  ** array is allocated to contain the "id" field from the vfile table
  ** for each file to be committed. Or, if aCommitFile is NULL, all files
  ** should be committed.
  */
  select_commit_files(db);
  isAMerge = db_exists(db, "SELECT 1 FROM vmerge");
  if( db->aCommitFile && isAMerge ){
    geomvcs_fatal(db, "cannot do a partial commit of a merge");
  }

  user_select();
  /*
  ** Check that the user exists.
  */
  if( !db_exists(db, "SELECT 1 FROM user WHERE login=%Q", db->zLogin) ){
    geomvcs_fatal(db, "no such user: %s", db->zLogin);
  }
  
  hasChanges = unsaved_changes();
  db_begin_transaction(db);
  db_record_repository_filename(0);
  if( hasChanges==0 && !isAMerge && !forceFlag ){
    geomvcs_fatal(db, "nothing has changed");
  }

  /* If one or more files that were named on the command line have not
  ** been modified, bail out now.
  */
  if( db->aCommitFile ){
    Blob unmodified;
    memset(&unmodified, 0, sizeof(Blob));
    blob_init(&unmodified, 0, 0);
    db_blob(&unmodified, 
      "SELECT pathname FROM vfile"
      " WHERE chnged = 0 AND origname IS NULL AND file_is_selected(id)"
    );
    if( strlen(blob_str(db, &unmodified)) ){
      geomvcs_fatal(db, "file %s has not changed", blob_str(db, &unmodified));
    }
  }

  /*
  ** Do not allow a commit that will cause a fork unless the --force flag
  ** is used or unless this is a private check-in.
  */
  if( zBranch==0 && forceFlag==0 && db->markPrivate==0 && !is_a_leaf(db, vid) ){
    geomvcs_fatal(db, "would fork.  \"update\" first or use -f or --force.");
  }

  /*
  ** Do not allow a commit against a closed leaf 
  */
  if( db_exists(db, "SELECT 1 FROM tagxref"
                " WHERE tagid=%d AND rid=%d AND tagtype>0",
                TAG_CLOSED, vid) ){
    geomvcs_fatal(db, "cannot commit against a closed leaf");
  }

  if( useCksum ) vfile_aggregate_checksum_disk(vid, &cksum1);
  if( zComment ){
    blob_zero(&comment);
    blob_append(db, &comment, zComment, -1);
  }else if( zComFile ){
    blob_zero(&comment);
    blob_read_from_file(db, &comment, zComFile);
  }else{
    char *zInit = db_text(db, 0, "SELECT value FROM vvar WHERE name='ci-comment'");
    prepare_commit_comment(db, &comment, zInit, zBranch, vid);
    free(zInit);
  }
  if( blob_size(&comment)==0 ){
    Blob ans;
    blob_zero(&ans);
    prompt_user("empty check-in comment.  continue (y/N)? ", &ans);
    if( blob_str(db, &ans)[0]!='y' ){
      geomvcs_exit(db, 1);
    }
  }else{
    db_multi_exec(db, "REPLACE INTO vvar VALUES('ci-comment',%B)", &comment);
    db_end_transaction(db, 0);
    db_begin_transaction(db);
  }

  /* Step 1: Insert records for all modified files into the blob 
  ** table. If there were arguments passed to this command, only
  ** the identified fils are inserted (if they have been modified).
  */
  db_prepare(db, &q,
    "SELECT id, %Q || pathname, mrid, %s FROM vfile "
    "WHERE chnged==1 AND NOT deleted AND file_is_selected(id)",
    db->zLocalRoot, glob_expr(db, "pathname", db_get(db, "crnl-glob",""))
  );
  while( db_step(&q)==SQLITE_ROW ){
    int id, rid;
    const char *zFullname;
    Blob content;
    int crnlOk;

    id = db_column_int(&q, 0);
    zFullname = db_column_text(&q, 1);
    rid = db_column_int(&q, 2);
    crnlOk = db_column_int(&q, 3);

    blob_zero(&content);
    blob_read_from_file(db, &content, zFullname);
    if( !crnlOk ) cr_warning(db, &content, zFullname);
    nrid = content_put(&content);
    blob_reset(db, &content);
    if( rid>0 ){
      content_deltify(rid, nrid, 0);
    }
    db_multi_exec(db, "UPDATE vfile SET mrid=%d, rid=%d WHERE id=%d", nrid,nrid,id);
    db_multi_exec(db, "INSERT OR IGNORE INTO unsent VALUES(%d)", nrid);
  }
  db_finalize(db, &q);

  /* Create the new manifest */
  if( blob_size(&comment)==0 ){
    blob_append(db, &comment, "(no comment)", -1);
  }
  if( forceDelta ){
    blob_zero(&manifest);
  }else{
    create_manifest(db, &manifest, 0, 0, &comment, vid,
                    !forceFlag, useCksum ? &cksum1 : 0,
                    zDateOvrd, zUserOvrd, zBranch, zBgColor, zTag, &szB);
  }

  /* See if a delta-manifest would be more appropriate */
  if( !forceBaseline ){
    const char *zBaselineUuid;
    Manifest *pParent;
    Manifest *pBaseline;
    pParent = manifest_get(vid, CFTYPE_MANIFEST);
    if( pParent && pParent->zBaseline ){
      zBaselineUuid = pParent->zBaseline;
      pBaseline = manifest_get_by_name(zBaselineUuid, 0);
    }else{
      zBaselineUuid = db_text(db, 0, "SELECT uuid FROM blob WHERE rid=%d", vid);
      pBaseline = pParent;
    }
    if( pBaseline ){
      Blob delta;
      create_manifest(db, &delta, zBaselineUuid, pBaseline, &comment, vid,
                      !forceFlag, useCksum ? &cksum1 : 0,
                      zDateOvrd, zUserOvrd, zBranch, zBgColor, zTag, &szD);
      /*
      ** At this point, two manifests have been constructed, either of
      ** which would work for this checkin.  The first manifest (held
      ** in the "manifest" variable) is a baseline manifest and the second
      ** (held in variable named "delta") is a delta manifest.  The
      ** question now is: which manifest should we use?
      **
      ** Let B be the number of F-cards in the baseline manifest and
      ** let D be the number of F-cards in the delta manifest, plus one for
      ** the B-card.  (B is held in the szB variable and D is held in the
      ** szD variable.)  Assume that all delta manifests adds X new F-cards.
      ** Then to minimize the total number of F- and B-cards in the repository,
      ** we should use the delta manifest if and only if:
      **
      **      D*D < B*X - X*X
      **
      ** X is an unknown here, but for most repositories, we will not be
      ** far wrong if we assume X=3.
      */
      if( forceDelta || (szD*szD)<(szB*3-9) ){
        blob_reset(db, &manifest);
        manifest = delta;
      }else{
        blob_reset(db, &delta);
      }
    }else if( forceDelta ){
      geomvcs_panic(db, "unable to find a baseline-manifest for the delta");
    }
  }
  if( !noSign && !db->markPrivate && clearsign(&manifest, &manifest) ){
    Blob ans;
    blob_zero(&ans);
    prompt_user("unable to sign manifest.  continue (y/N)? ", &ans);
    if( blob_str(db, &ans)[0]!='y' ){
      geomvcs_exit(db, 1);
    }
  }

  /* If the --test option is specified, output the manifest file
  ** and rollback the transaction.  
  */
  if( testRun ){
    blob_write_to_file(&manifest, "");
  }

  if( outputManifest ){
    zManifestFile = mprintf(db, "%smanifest", db->zLocalRoot);
    blob_write_to_file(&manifest, zManifestFile);
    blob_reset(db, &manifest);
    blob_read_from_file(db, &manifest, zManifestFile);
    free(zManifestFile);
  }
  nvid = content_put(&manifest);
  if( nvid==0 ){
    geomvcs_panic(db, "trouble committing manifest: %s", db->zErrMsg);
  }
  db_multi_exec(db, "INSERT OR IGNORE INTO unsent VALUES(%d)", nvid);
  manifest_crosslink(nvid, &manifest);
  assert( blob_is_reset(&manifest) );
  content_deltify(vid, nvid, 0);
  zUuid = db_text(db, 0, "SELECT uuid FROM blob WHERE rid=%d", nvid);
  printf("New_Version: %s\n", zUuid);
  if( outputManifest ){
    zManifestFile = mprintf(db, "%smanifest.uuid", db->zLocalRoot);
    blob_zero(&muuid);
    blob_appendf(db, &muuid, "%s\n", zUuid);
    blob_write_to_file(&muuid, zManifestFile);
    free(zManifestFile);
    blob_reset(db, &muuid);
  }

  
  /* Update the vfile and vmerge tables */
  db_multi_exec(db, 
    "DELETE FROM vfile WHERE (vid!=%d OR deleted) AND file_is_selected(id);"
    "DELETE FROM vmerge WHERE file_is_selected(id) OR id=0;"
    "UPDATE vfile SET vid=%d;"
    "UPDATE vfile SET rid=mrid, chnged=0, deleted=0, origname=NULL"
    " WHERE file_is_selected(id);"
    , vid, nvid
  );
  db_lset_int("checkout", nvid);

  if( useCksum ){
    /* Verify that the repository checksum matches the expected checksum
    ** calculated before the checkin started (and stored as the R record
    ** of the manifest file).
    */
    vfile_aggregate_checksum_repository(nvid, &cksum2);
    if( blob_compare(&cksum1, &cksum2) ){
      vfile_compare_repository_to_disk(nvid);
      geomvcs_fatal(db, "working checkout does not match what would have ended "
                   "up in the repository:  %b versus %b",
                   &cksum1, &cksum2);
    }
  
    /* Verify that the manifest checksum matches the expected checksum */
    vfile_aggregate_checksum_manifest(nvid, &cksum2, &cksum1b);
    if( blob_compare(&cksum1, &cksum1b) ){
      geomvcs_fatal(db, "manifest checksum self-test failed: "
                   "%b versus %b", &cksum1, &cksum1b);
    }
    if( blob_compare(&cksum1, &cksum2) ){
      geomvcs_fatal(db, 
         "working checkout does not match manifest after commit: "
         "%b versus %b", &cksum1, &cksum2);
    }
  
    /* Verify that the commit did not modify any disk images. */
    vfile_aggregate_checksum_disk(nvid, &cksum2);
    if( blob_compare(&cksum1, &cksum2) ){
      geomvcs_fatal(db, "working checkout before and after commit does not match");
    }
  }

  /* Clear the undo/redo stack */
  undo_reset();

  /* Commit */
  db_multi_exec(db, "DELETE FROM vvar WHERE name='ci-comment'");
  if( testRun ){
    db_end_transaction(db, 1);
    exit(1);
  }
  db_end_transaction(db, 0);

  if( !db->markPrivate ){
    autosync(AUTOSYNC_PUSH);  
  }
  if( count_nonbranch_children(vid)>1 ){
    printf("**** warning: a fork has occurred *****\n");
  }
}
