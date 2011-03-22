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
** Procedures store and retrieve records from the repository
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/basic.h"
#include "geomvcs/blob.h"
#include "geomvcs/bag.h"
#include "geomvcs/db.h"
#include <assert.h>

/*
** The artifact retrival cache
*/
static struct {
  i64 szTotal;         /* Total size of all entries in the cache */
  int n;               /* Current number of eache entries */
  int nAlloc;          /* Number of slots allocated in a[] */
  int nextAge;         /* Age counter for implementing LRU */
  int skipCnt;         /* Used to limit entries expelled from cache */
  struct cacheLine {   /* One instance of this for each cache entry */
    int rid;                  /* Artifact id */
    int age;                  /* Age.  Newer is larger */
    Blob content;             /* Content of the artifact */
  } *a;                /* The positive cache */
  Bag inCache;         /* Set of artifacts currently in cache */

  /*
  ** The missing artifact cache.
  **
  ** Artifacts whose record ID are in missingCache cannot be retrieved
  ** either because they are phantoms or because they are a delta that
  ** depends on a phantom.  Artifacts whose content we are certain is
  ** available are in availableCache.  If an artifact is in neither cache
  ** then its current availablity is unknown.
  */
  Bag missing;         /* Cache of artifacts that are incomplete */
  Bag available;       /* Cache of artifacts that are complete */
} contentCache;

/*
** Remove the oldest element from the content cache
*/
static void content_cache_expire_oldest(struct vcs_db *db){
  int i;
  int mnAge = contentCache.nextAge;
  int mn = -1;
  for(i=0; i<contentCache.n; i++){
    if( contentCache.a[i].age<mnAge ){
      mnAge = contentCache.a[i].age;
      mn = i;
    }
  }
  if( mn>=0 ){
    bag_remove(db, &contentCache.inCache, contentCache.a[mn].rid);
    contentCache.szTotal -= blob_size(&contentCache.a[mn].content);
    blob_reset(db, &contentCache.a[mn].content);
    contentCache.n--;
    contentCache.a[mn] = contentCache.a[contentCache.n];
  }
}

/*
** Add an entry to the content cache.
**
** This routines hands responsibility for the artifact over to the cache.
** The cache will deallocate memory when it has finished with it.
*/
void content_cache_insert(struct vcs_db *db, int rid, Blob *pBlob){
  struct cacheLine *p;
  if( contentCache.n>500 || contentCache.szTotal>50000000 ){
    content_cache_expire_oldest(db);
  }
  if( contentCache.n>=contentCache.nAlloc ){
    contentCache.nAlloc = contentCache.nAlloc*2 + 10;
    contentCache.a = geomvcs_realloc(db, contentCache.a,
                             contentCache.nAlloc*sizeof(contentCache.a[0]));
  }
  p = &contentCache.a[contentCache.n++];
  p->rid = rid;
  p->age = contentCache.nextAge++;
  contentCache.szTotal += blob_size(pBlob);
  p->content = *pBlob;
  blob_zero(pBlob);
  bag_insert(db, &contentCache.inCache, rid);
}

/*
** Clear the content cache.
*/
void content_clear_cache(struct vcs_db *db){
  int i;
  for(i=0; i<contentCache.n; i++){
    blob_reset(db, &contentCache.a[i].content);
  }
  bag_clear(&contentCache.missing);
  bag_clear(&contentCache.available);
  bag_clear(&contentCache.inCache);
  contentCache.n = 0;
  contentCache.szTotal = 0;
}

/*
** Return the srcid associated with rid.  Or return 0 if rid is 
** original content and not a delta.
*/
static int findSrcid(struct vcs_db *db, int rid){
  static Stmt q;
  int srcid;
  db_static_prepare(db, &q, "SELECT srcid FROM delta WHERE rid=:rid");
  db_bind_int(db, &q, ":rid", rid);
  if( db_step(&q)==SQLITE_ROW ){
    srcid = db_column_int(&q, 0);
  }else{
    srcid = 0;
  }
  db_reset(db, &q);
  return srcid;
}

/*
** Return the blob.size field given blob.rid
*/
int content_size(struct vcs_db *db, int rid, int dflt){
  static Stmt q;
  int sz = dflt;
  db_static_prepare(db, &q, "SELECT size FROM blob WHERE rid=:r");
  db_bind_int(db, &q, ":r", rid);
  if( db_step(&q)==SQLITE_ROW ){
    sz = db_column_int(&q, 0);
  }
  db_reset(db, &q);
  return sz;
}

/*
** Check to see if content is available for artifact "rid".  Return
** true if it is.  Return false if rid is a phantom or depends on
** a phantom.
*/
int content_is_available(struct vcs_db *db, int rid){
  int srcid;
  int depth = 0;  /* Limit to recursion depth */
  while( depth++ < 10000000 ){  
    if( bag_find(&contentCache.missing, rid) ){
      return 0;
    }
    if( bag_find(&contentCache.available, rid) ){
      return 1;
    }
    if( content_size(db, rid, -1)<0 ){
      bag_insert(db, &contentCache.missing, rid);
      return 0;
    }
    srcid = findSrcid(db, rid);
    if( srcid==0 ){
      bag_insert(db, &contentCache.available, rid);
      return 1;
    }
    rid = srcid;
  }
  geomvcs_panic(db, "delta-loop in repository");
  return 0;
}

/*
** Mark artifact rid as being available now.  Update the cache to
** show that everything that was formerly unavailable because rid
** was missing is now available.
*/
static void content_mark_available(struct vcs_db *db, int rid){
  Bag pending;
  static Stmt q;
  if( bag_find(&contentCache.available, rid) ) return;
  bag_init(&pending);
  bag_insert(db, &pending, rid);
  while( (rid = bag_first(&pending))!=0 ){
    bag_remove(db, &pending, rid);
    bag_remove(db, &contentCache.missing, rid);
    bag_insert(db, &contentCache.available, rid);
    db_static_prepare(db, &q, "SELECT rid FROM delta WHERE srcid=:rid");
    db_bind_int(db, &q, ":rid", rid);
    while( db_step(&q)==SQLITE_ROW ){
      int nx = db_column_int(&q, 0);
      bag_insert(db, &pending, nx);
    }
    db_reset(db, &q);
  }
  bag_clear(&pending);
}

/*
** Get the blob.content value for blob.rid=rid.  Return 1 on success or
** 0 on failure.
*/
static int content_of_blob(struct vcs_db *db, int rid, Blob *pBlob){
  static Stmt q;
  int rc = 0;
  db_static_prepare(db, &q, "SELECT content FROM blob WHERE rid=:rid AND size>=0");
  db_bind_int(db, &q, ":rid", rid);
  if( db_step(&q)==SQLITE_ROW ){
    db_ephemeral_blob(&q, 0, pBlob);
    blob_uncompress(db, pBlob, pBlob);
    rc = 1;
  }
  db_reset(db, &q);
  return rc;
}

/*
** Extract the content for ID rid and put it into the
** uninitialized blob.  Return 1 on success.  If the record
** is a phantom, zero pBlob and return 0.
*/
int content_get(struct vcs_db *db, int rid, Blob *pBlob){
  int rc;
  int i;
  int nextRid;

  assert( db->repositoryOpen );
  blob_zero(pBlob);
  if( rid==0 ) return 0;

  /* Early out if we know the content is not available */
  if( bag_find(&contentCache.missing, rid) ){
    return 0;
  }

  /* Look for the artifact in the cache first */
  if( bag_find(&contentCache.inCache, rid) ){
    for(i=0; i<contentCache.n; i++){
      if( contentCache.a[i].rid==rid ){
        blob_copy(db, pBlob, &contentCache.a[i].content);
        contentCache.a[i].age = contentCache.nextAge++;
        return 1;
      }
    }
  }

  nextRid = findSrcid(db, rid);
  if( nextRid==0 ){
    rc = content_of_blob(db, rid, pBlob);
  }else{
    int n = 1;
    int nAlloc = 10;
    int *a = 0;
    int mx;
    Blob delta, next;

    a = geomvcs_malloc(db, sizeof(a[0])*nAlloc );
    a[0] = rid;
    a[1] = nextRid;
    n = 1;
    while( !bag_find(&contentCache.inCache, nextRid)
        && (nextRid = findSrcid(db, nextRid))>0 ){
      n++;
      if( n>=nAlloc ){
        nAlloc = nAlloc*2 + 10;
        a = geomvcs_realloc(db, a, nAlloc*sizeof(a[0]));
      }
      a[n] = nextRid;
    }
    mx = n;
    rc = content_get(db, a[n], pBlob);
    n--;
    while( rc && n>=0 ){
      rc = content_of_blob(db, a[n], &delta);
      if( rc ){
        blob_delta_apply(pBlob, &delta, &next);
        blob_reset(db, &delta);
        if( (mx-n)%8==0 ){
          content_cache_insert(db, a[n+1], pBlob);
        }else{
          blob_reset(db, pBlob);
        }
        *pBlob = next;
      }
      n--;
    }
    free(a);
    if( !rc ) blob_reset(db, pBlob);
  }
  if( rc==0 ){
    bag_insert(db, &contentCache.missing, rid);
  }else{
    bag_insert(db, &contentCache.available, rid);
  }
  return rc;
}

/*
** The following flag is set to disable the automatic calls to
** manifest_crosslink() when a record is dephantomized.  This
** flag can be set (for example) when doing a clone when we know
** that rebuild will be run over all records at the conclusion
** of the operation.
*/
static int ignoreDephantomizations = 0;

/*
** When a record is converted from a phantom to a real record,
** if that record has other records that are derived by delta,
** then call manifest_crosslink() on those other records.
**
** If the formerly phantom record or any of the other records
** derived by delta from the former phantom are a baseline manifest,
** then also invoke manifest_crosslink() on the delta-manifests
** associated with that baseline.
**
** Tail recursion is used to minimize stack depth.
*/
void after_dephantomize(struct vcs_db *db, int rid, int linkFlag){
  Stmt q;
  int nChildAlloc = 0;
  int *aChild = 0;
  Blob content;

  if( ignoreDephantomizations ) return;
  while( rid ){
    int nChildUsed = 0;
    int i;

    /* Parse the object rid itself */
    if( linkFlag ){
      content_get(db, rid, &content);
      manifest_crosslink(rid, &content);
      assert( blob_is_reset(&content) );
    }

    /* Parse all delta-manifests that depend on baseline-manifest rid */
    db_prepare(db, &q, "SELECT rid FROM orphan WHERE baseline=%d", rid);
    while( db_step(&q)==SQLITE_ROW ){
      int child = db_column_int(&q, 0);
      if( nChildUsed>=nChildAlloc ){
        nChildAlloc = nChildAlloc*2 + 10;
        aChild = geomvcs_realloc(db, aChild, nChildAlloc*sizeof(aChild));
      }
      aChild[nChildUsed++] = child;
    }
    db_finalize(db, &q);
    for(i=0; i<nChildUsed; i++){
      content_get(db, aChild[i], &content);
      manifest_crosslink(aChild[i], &content);
      assert( blob_is_reset(&content) );
    }
    if( nChildUsed ){
      db_multi_exec(db, "DELETE FROM orphan WHERE baseline=%d", rid);
    }

    /* Recursively dephantomize all artifacts that are derived by
    ** delta from artifact rid and which have not already been
    ** cross-linked.  */
    nChildUsed = 0;
    db_prepare(db, &q, 
       "SELECT rid FROM delta"
       " WHERE srcid=%d"
       "   AND NOT EXISTS(SELECT 1 FROM mlink WHERE mid=delta.rid)",
       rid
    );
    while( db_step(&q)==SQLITE_ROW ){
      int child = db_column_int(&q, 0);
      if( nChildUsed>=nChildAlloc ){
        nChildAlloc = nChildAlloc*2 + 10;
        aChild = geomvcs_realloc(db, aChild, nChildAlloc*sizeof(aChild));
      }
      aChild[nChildUsed++] = child;
    }
    db_finalize(db, &q);
    for(i=1; i<nChildUsed; i++){
      after_dephantomize(db, aChild[i], 1);
    }

    /* Tail recursion for the common case where only a single artifact
    ** is derived by delta from rid... */
    rid = nChildUsed>0 ? aChild[0] : 0;
    linkFlag = 1;
  }
  free(aChild);
}

/*
** Turn dephantomization processing on or off.
*/
void content_enable_dephantomize(int onoff){
  ignoreDephantomizations = !onoff;
}

/*
** Write content into the database.  Return the record ID.  If the
** content is already in the database, just return the record ID.
**
** If srcId is specified, then pBlob is delta content from
** the srcId record.  srcId might be a phantom.  
**
** pBlob is normally uncompressed text.  But if nBlob>0 then the
** pBlob value has already been compressed and nBlob is its uncompressed
** size.  If nBlob>0 then zUuid must be valid.
**
** zUuid is the UUID of the artifact, if it is specified.  When srcId is
** specified then zUuid must always be specified.  If srcId is zero,
** and zUuid is zero then the correct zUuid is computed from pBlob.
**
** If the record already exists but is a phantom, the pBlob content
** is inserted and the phatom becomes a real record.
**
** The original content of pBlob is not disturbed.  The caller continues
** to be responsible for pBlob.  This routine does *not* take over
** responsiblity for freeing pBlob.
*/
int content_put_ex(
  struct vcs_db *db,
  Blob *pBlob,              /* Content to add to the repository */
  const char *zUuid,        /* SHA1 hash of reconstructed pBlob */
  int srcId,                /* pBlob is a delta from this entry */
  int nBlob,                /* pBlob is compressed. Original size is this */
  int isPrivate             /* The content should be marked private */
){
  int size;
  int rid;
  Stmt s1;
  Blob cmpr;
  Blob hash;
  int markAsUnclustered = 0;
  int isDephantomize = 0;
  
  assert( db->repositoryOpen );
  assert( pBlob!=0 );
  assert( srcId==0 || zUuid!=0 );
  if( zUuid==0 ){
    assert( pBlob!=0 );
    assert( nBlob==0 );
    sha1sum_blob(pBlob, &hash);
  }else{
    blob_init(&hash, zUuid, -1);
  }
  if( nBlob ){
    size = nBlob;
  }else{
    size = blob_size(pBlob);
    if( srcId ){
      size = delta_output_size(blob_buffer(pBlob), size);
    }
  }
  db_begin_transaction(db);

  /* Check to see if the entry already exists and if it does whether
  ** or not the entry is a phantom
  */
  db_prepare(db, &s1, "SELECT rid, size FROM blob WHERE uuid=%B", &hash);
  if( db_step(&s1)==SQLITE_ROW ){
    rid = db_column_int(&s1, 0);
    if( db_column_int(&s1, 1)>=0 || pBlob==0 ){
      /* Either the entry is not a phantom or it is a phantom but we
      ** have no data with which to dephantomize it.  In either case,
      ** there is nothing for us to do other than return the RID. */
      db_finalize(db, &s1);
      db_end_transaction(db, 0);
      return rid;
    }
  }else{
    rid = 0;  /* No entry with the same UUID currently exists */
    markAsUnclustered = 1;
  }
  db_finalize(db, &s1);

  /* Construct a received-from ID if we do not already have one */
  if( db->rcvid==0 ){
    db_multi_exec(db, 
       "INSERT INTO rcvfrom(uid, mtime, nonce, ipaddr)"
       "VALUES(%d, julianday('now'), %Q, %Q)",
       db->userUid, db->zNonce, db->zIpAddr
    );
    db->rcvid = db_last_insert_rowid(db);
  }

  if( nBlob ){
    cmpr = pBlob[0];
  }else{
    blob_compress(pBlob, &cmpr);
  }
  if( rid>0 ){
    /* We are just adding data to a phantom */
    db_prepare(db, &s1,
      "UPDATE blob SET rcvid=%d, size=%d, content=:data WHERE rid=%d",
       db->rcvid, size, rid
    );
    db_bind_blob(&s1, ":data", &cmpr);
    db_exec(db, &s1);
    db_multi_exec(db, "DELETE FROM phantom WHERE rid=%d", rid);
    if( srcId==0 || content_is_available(db, srcId) ){
      isDephantomize = 1;
      content_mark_available(db, rid);
    }
  }else{
    /* We are creating a new entry */
    db_prepare(db, &s1,
      "INSERT INTO blob(rcvid,size,uuid,content)"
      "VALUES(%d,%d,'%b',:data)",
       db->rcvid, size, &hash
    );
    db_bind_blob(&s1, ":data", &cmpr);
    db_exec(db, &s1);
    rid = db_last_insert_rowid(db);
    if( !pBlob ){
      db_multi_exec(db, "INSERT OR IGNORE INTO phantom VALUES(%d)", rid);
    }
    if( db->markPrivate || isPrivate ){
      db_multi_exec(db, "INSERT INTO private VALUES(%d)", rid);
      markAsUnclustered = 0;
    }
  }
  if( nBlob==0 ) blob_reset(db, &cmpr);

  /* If the srcId is specified, then the data we just added is
  ** really a delta.  Record this fact in the delta table.
  */
  if( srcId ){
    db_multi_exec(db, "REPLACE INTO delta(rid,srcid) VALUES(%d,%d)", rid, srcId);
  }
  if( !isDephantomize && bag_find(&contentCache.missing, rid) && 
      (srcId==0 || content_is_available(db, srcId)) ){
    content_mark_available(db, rid);
  }
  if( isDephantomize ){
    after_dephantomize(db, rid, 0);
  }
  
  /* Add the element to the unclustered table if has never been
  ** previously seen.
  */
  if( markAsUnclustered ){
    db_multi_exec(db, "INSERT OR IGNORE INTO unclustered VALUES(%d)", rid);
  }

  /* Finish the transaction and cleanup */
  db_finalize(db, &s1);
  db_end_transaction(db, 0);
  blob_reset(db, &hash);

  /* Make arrangements to verify that the data can be recovered
  ** before we commit */
  verify_before_commit(rid);
  return rid;
}

/*
** This is the simple common case for inserting content into the
** repository.  pBlob is the content to be inserted.
**
** pBlob is uncompressed and is not deltaed.  It is exactly the content
** to be inserted.
**
** The original content of pBlob is not disturbed.  The caller continues
** to be responsible for pBlob.  This routine does *not* take over
** responsiblity for freeing pBlob.
*/
int content_put(struct vcs_db *db, Blob *pBlob){
  return content_put_ex(db, pBlob, 0, 0, 0, 0);
}


/*
** Create a new phantom with the given UUID and return its artifact ID.
*/
int content_new(struct vcs_db *db, const char *zUuid, int isPrivate){
  int rid;
  static Stmt s1, s2, s3;
  
  assert( db->repositoryOpen );
  db_begin_transaction(db);
  if( uuid_is_shunned(zUuid) ){
    db_end_transaction(db, 0);
    return 0;
  }
  db_static_prepare(db, &s1,
    "INSERT INTO blob(rcvid,size,uuid,content)"
    "VALUES(0,-1,:uuid,NULL)"
  );
  db_bind_text(db, &s1, ":uuid", zUuid);
  db_exec(db, &s1);
  rid = db_last_insert_rowid(db);
  db_static_prepare(db, &s2,
    "INSERT INTO phantom VALUES(:rid)"
  );
  db_bind_int(db, &s2, ":rid", rid);
  db_exec(db, &s2);
  if( db->markPrivate || isPrivate ){
    db_multi_exec(db, "INSERT INTO private VALUES(%d)", rid);
  }else{
    db_static_prepare(db, &s3,
      "INSERT INTO unclustered VALUES(:rid)"
    );
    db_bind_int(db, &s3, ":rid", rid);
    db_exec(db, &s3);
  }
  bag_insert(db, &contentCache.missing, rid);
  db_end_transaction(db, 0);
  return rid;
}

/*
** Make sure the content at rid is the original content and is not a
** delta.
*/
void content_undelta(struct vcs_db *db, int rid){
  if( findSrcid(db, rid)>0 ){
    Blob x;
    if( content_get(db, rid, &x) ){
      Stmt s;
      db_prepare(db, &s, "UPDATE blob SET content=:c, size=%d WHERE rid=%d",
                     blob_size(&x), rid);
      blob_compress(&x, &x);
      db_bind_blob(&s, ":c", &x);
      db_exec(db, &s);
      db_finalize(db, &s);
      blob_reset(db, &x);
      db_multi_exec(db, "DELETE FROM delta WHERE rid=%d", rid);
    }
  }
}

/*
** Return true if the given RID is marked as PRIVATE.
*/
int content_is_private(struct vcs_db *db, int rid){
  static Stmt s1;
  int rc;
  db_static_prepare(db, &s1,
    "SELECT 1 FROM private WHERE rid=:rid"
  );
  db_bind_int(db, &s1, ":rid", rid);
  rc = db_step(&s1);
  db_reset(db, &s1);
  return rc==SQLITE_ROW;  
}

/*
** Make sure an artifact is public.  
*/
void content_make_public(struct vcs_db *db, int rid){
  static Stmt s1;
  db_static_prepare(db, &s1,
    "DELETE FROM private WHERE rid=:rid"
  );
  db_bind_int(db, &s1, ":rid", rid);
  db_exec(db, &s1);
}

/*
** Change the storage of rid so that it is a delta of srcid.
**
** If rid is already a delta from some other place then no
** conversion occurs and this is a no-op unless force==1.
**
** Never generate a delta that carries a private artifact into a public
** artifact.  Otherwise, when we go to send the public artifact on a
** sync operation, the other end of the sync will never be able to receive
** the source of the delta.  It is OK to delta private->private and
** public->private and public->public.  Just no private->public delta.
**
** If srcid is a delta that depends on rid, then srcid is
** converted to undeltaed text.
**
** If either rid or srcid contain less than 50 bytes, or if the
** resulting delta does not achieve a compression of at least 25% 
** the rid is left untouched.
**
** Return 1 if a delta is made and 0 if no delta occurs.
*/
int content_deltify(struct vcs_db *db, int rid, int srcid, int force){
  int s;
  Blob data, src, delta;
  Stmt s1, s2;
  int rc = 0;

  if( srcid==rid ) return 0;
  if( !force && findSrcid(db, rid)>0 ) return 0;
  if( content_is_private(db, srcid) && !content_is_private(db, rid) ){
    return 0;
  }
  s = srcid;
  while( (s = findSrcid(db, s))>0 ){
    if( s==rid ){
      content_undelta(db, srcid);
      break;
    }
  }
  content_get(db, srcid, &src);
  if( blob_size(&src)<50 ){
    blob_reset(db, &src);
    return 0;
  }
  content_get(db, rid, &data);
  if( blob_size(&data)<50 ){
    blob_reset(db, &src);
    blob_reset(db, &data);
    return 0;
  }
  blob_delta_create(db, &src, &data, &delta);
  if( blob_size(&delta) <= blob_size(&data)*0.75 ){
    blob_compress(&delta, &delta);
    db_prepare(db, &s1, "UPDATE blob SET content=:data WHERE rid=%d", rid);
    db_prepare(db, &s2, "REPLACE INTO delta(rid,srcid)VALUES(%d,%d)", rid, srcid);
    db_bind_blob(&s1, ":data", &delta);
    db_begin_transaction(db);
    db_exec(db, &s1);
    db_exec(db, &s2);
    db_end_transaction(db, 0);
    db_finalize(db, &s1);
    db_finalize(db, &s2);
    verify_before_commit(rid);
    rc = 1;
  }
  blob_reset(db, &src);
  blob_reset(db, &data);
  blob_reset(db, &delta);
  return rc;
}
