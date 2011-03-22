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
** This file contains code used to implement a priority queue.
** A priority queue is a list of items order by a floating point
** value.  We can insert integers with each integer tied to its
** value then extract the integer with the smallest value.
**
** The way this queue is used, we never expect it to contain more
** than 2 or 3 elements, so a simple array is sufficient as the
** implementation.  This could give worst case O(N) insert times,
** but because of the nature of the problem we expect O(1) performance.
*/
#include "geomvcs/config.h"
#include "geomvcs/common.h"
#include "geomvcs/pqueue.h"
#include <assert.h>

/* Forward declarations for pqueue.c */
void *geomvcs_realloc(struct vcs_db *db, void *p,size_t n);

/*
** Initialize a PQueue structure
*/
void pqueue_init(PQueue *p){
  memset(p, 0, sizeof(*p));
}

/*
** Destroy a PQueue.  Delete all of its content.
*/
void pqueue_clear(PQueue *p){
  free(p->a);
  pqueue_init(p);
}

/*
** Change the size of the queue so that it contains N slots
*/
static void pqueue_resize(struct vcs_db *db, PQueue *p, int N){
  p->a = geomvcs_realloc(db, p->a, sizeof(p->a[0])*N);
  p->sz = N;
}

/*
** Insert element e into the queue.
*/
void pqueue_insert(struct vcs_db *db, PQueue *p, int e, double v, void *pData){
  int i, j;
  if( p->cnt+1>p->sz ){
    pqueue_resize(db, p, p->cnt+5);
  }
  for(i=0; i<p->cnt; i++){
    if( p->a[i].value>v ){
      for(j=p->cnt; j>i; j--){
        p->a[j] = p->a[j-1];
      }
      break;
    }
  }
  p->a[i].id = e;
  p->a[i].p = pData;
  p->a[i].value = v;
  p->cnt++;
}

/*
** Extract the first element from the queue (the element with
** the smallest value) and return its ID.  Return 0 if the queue
** is empty.
*/
int pqueue_extract(PQueue *p, void **pp){
  int e, i;
  if( p->cnt==0 ){
    if( pp ) *pp = 0;
    return 0;
  }
  e = p->a[0].id;
  if( pp ) *pp = p->a[0].p;
  for(i=0; i<p->cnt-1; i++){
    p->a[i] = p->a[i+1];
  }
  p->cnt--;
  return e;
}
