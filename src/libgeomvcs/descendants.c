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
** This file contains code used to find decendants of a version
** or leaves of a version tree.
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/blob.h"
#include "geomvcs/bag.h"
#include "geomvcs/schema.h"
#include "geomvcs/pqueue.h"
#include "geomvcs/db.h"
#include <assert.h>


/*
** Create a temporary table named "leaves" if it does not
** already exist.  Load this table with the RID of all
** check-ins that are leaves which are decended from
** check-in iBase.
**
** A "leaf" is a check-in that has no children in the same branch.
** There is a separate permanent table LEAF that contains all leaves
** in the tree.  This routine is used to compute a subset of that
** table consisting of leaves that are descended from a single checkin.
**
** The closeMode flag determines behavior associated with the "closed"
** tag:
**
**    closeMode==0       Show all leaves regardless of the "closed" tag.
**
**    closeMode==1       Show only leaves without the "closed" tag.
**
**    closeMode==2       Show only leaves with the "closed" tag.
**
** The default behavior is to ignore closed leaves (closeMode==0).  To
** Show all leaves, use closeMode==1.  To show only closed leaves, use
** closeMode==2.
*/
void compute_leaves(struct vcs_db *db, int iBase, int closeMode){

  /* Create the LEAVES table if it does not already exist.  Make sure
  ** it is empty.
  */
  db_multi_exec(db,
    "CREATE TEMP TABLE IF NOT EXISTS leaves("
    "  rid INTEGER PRIMARY KEY"
    ");"
    "DELETE FROM leaves;"
  );

  if( iBase>0 ){
    Bag seen;     /* Descendants seen */
    Bag pending;  /* Unpropagated descendants */
    Stmt q1;      /* Query to find children of a check-in */
    Stmt isBr;    /* Query to check to see if a check-in starts a new branch */
    Stmt ins;     /* INSERT statement for a new record */

    /* Initialize the bags. */
    bag_init(&seen);
    bag_init(&pending);
    bag_insert(db, &pending, iBase);

    /* This query returns all non-branch-merge children of check-in :rid.
    **
    ** If a a child is a merge of a fork within the same branch, it is 
    ** returned.  Only merge children in different branches are excluded.
    */
    db_prepare(db, &q1,
      "SELECT cid FROM plink"
      " WHERE pid=:rid"
      "   AND (isprim"
      "        OR coalesce((SELECT value FROM tagxref"
                        "   WHERE tagid=%d AND rid=plink.pid), 'trunk')"
                 "=coalesce((SELECT value FROM tagxref"
                        "   WHERE tagid=%d AND rid=plink.cid), 'trunk'))",
      TAG_BRANCH, TAG_BRANCH
    );
  
    /* This query returns a single row if check-in :rid is the first
    ** check-in of a new branch.
    */
    db_prepare(db, &isBr, 
       "SELECT 1 FROM tagxref"
       " WHERE rid=:rid AND tagid=%d AND tagtype=2"
       "   AND srcid>0",
       TAG_BRANCH
    );
  
    /* This statement inserts check-in :rid into the LEAVES table.
    */
    db_prepare(db, &ins, "INSERT OR IGNORE INTO leaves VALUES(:rid)");
  
    while( bag_count(&pending) ){
      int rid = bag_first(&pending);
      int cnt = 0;
      bag_remove(db, &pending, rid);
      db_bind_int(db, &q1, ":rid", rid);
      while( db_step(&q1)==SQLITE_ROW ){
        int cid = db_column_int(&q1, 0);
        if( bag_insert(db, &seen, cid) ){
          bag_insert(db, &pending, cid);
        }
        db_bind_int(db, &isBr, ":rid", cid);
        if( db_step(&isBr)==SQLITE_DONE ){
          cnt++;
        }
        db_reset(db, &isBr);
      }
      db_reset(db, &q1);
      if( cnt==0 && !is_a_leaf(rid) ){
        cnt++;
      }
      if( cnt==0 ){
        db_bind_int(db, &ins, ":rid", rid);
        db_step(&ins);
        db_reset(db, &ins);
      }
    }
    db_finalize(db, &ins);
    db_finalize(db, &isBr);
    db_finalize(db, &q1);
    bag_clear(&pending);
    bag_clear(&seen);
  }
  if( closeMode==1 ){
    db_multi_exec(db,
      "DELETE FROM leaves WHERE rid IN"
      "  (SELECT leaves.rid FROM leaves, tagxref"
      "    WHERE tagxref.rid=leaves.rid "
      "      AND tagxref.tagid=%d"
      "      AND tagxref.tagtype>0)",
      TAG_CLOSED
    );
  }else if( closeMode==2 ){
    db_multi_exec(db,
      "DELETE FROM leaves WHERE rid NOT IN"
      "  (SELECT leaves.rid FROM leaves, tagxref"
      "    WHERE tagxref.rid=leaves.rid "
      "      AND tagxref.tagid=%d"
      "      AND tagxref.tagtype>0)",
      TAG_CLOSED
    );
  }
}

/*
** Load the record ID rid and up to N-1 closest ancestors into
** the "ok" table.
*/
void compute_ancestors(struct vcs_db *db, int rid, int N){
  Bag seen;
  PQueue queue;
  bag_init(&seen);
  pqueue_init(&queue);
  bag_insert(db, &seen, rid);
  pqueue_insert(db, &queue, rid, 0.0, 0);
  while( (N--)>0 && (rid = pqueue_extract(&queue, 0))!=0 ){
    Stmt q;
    db_multi_exec(db, "INSERT OR IGNORE INTO ok VALUES(%d)", rid);
    db_prepare(db, &q,
       "SELECT a.pid, b.mtime FROM plink a LEFT JOIN plink b ON b.cid=a.pid"
       " WHERE a.cid=%d", rid
    );
    while( db_step(&q)==SQLITE_ROW ){
      int pid = db_column_int(&q, 0);
      double mtime = db_column_double(&q, 1);
      if( bag_insert(db, &seen, pid) ){
        pqueue_insert(db, &queue, pid, -mtime, 0);
      }
    }
    db_finalize(db, &q);
  }
  bag_clear(&seen);
  pqueue_clear(&queue);
}

/*
** Load the record ID rid and up to N-1 closest descendants into
** the "ok" table.
*/
void compute_descendants(struct vcs_db *db, int rid, int N){
  Bag seen;
  PQueue queue;
  bag_init(&seen);
  pqueue_init(&queue);
  bag_insert(db, &seen, rid);
  pqueue_insert(db, &queue, rid, 0.0, 0);
  while( (N--)>0 && (rid = pqueue_extract(&queue, 0))!=0 ){
    Stmt q;
    db_multi_exec(db, "INSERT OR IGNORE INTO ok VALUES(%d)", rid);
    db_prepare(db, &q,"SELECT cid, mtime FROM plink WHERE pid=%d", rid);
    while( db_step(&q)==SQLITE_ROW ){
      int pid = db_column_int(&q, 0);
      double mtime = db_column_double(&q, 1);
      if( bag_insert(db, &seen, pid) ){
        pqueue_insert(db, &queue, pid, mtime, 0);
      }
    }
    db_finalize(db, &q);
  }
  bag_clear(&seen);
  pqueue_clear(&queue);
}
