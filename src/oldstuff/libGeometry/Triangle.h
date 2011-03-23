/*                      T R I A N G L E . H
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file Triangle.h
 *
 * Brief description
 *
 */

#ifndef __TRIANGLE_H__
#define __TRIANGLE_H__

// system headers
#include <iostream>

/* interface headers */
#include "Geometry/Primitive.h"

#define EPSILON 0.000001
#define CROSS(dest,v1,v2) \
          dest[0]=v1[1]*v2[2]-v1[2]*v2[1]; \
          dest[1]=v1[2]*v2[0]-v1[0]*v2[2]; \
          dest[2]=v1[0]*v2[1]-v1[1]*v2[0];
#define DOT(v1,v2) (v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])
#define SUB(dest,v1,v2) \
dest[0]=v1[0]-v2[0]; \
dest[1]=v1[1]-v2[1]; \
dest[2]=v1[2]-v2[2]; 

namespace Geometry {
  // 128 uint8_ts
  typedef struct _triangle
  {
    double vertexA[3];
    double vertexB[3];
    double vertexC[3];
    double normal[3];
    unsigned long int padding[8];
  } Triangle;

  // 64 uint8_ts
  typedef struct _robustTriangle
  {
    double position[3];
    double normal[3];
    unsigned long int vertex[3];  // vertex index lookup
    unsigned long int padding;
  } RobustTriangle;

  // 32 uint8_ts
  typedef struct _traditionalTriangle 
  {
    float position[3];
    float normal[3];
    unsigned short int vertex[3]; // vertex index lookup
    unsigned short int padding;
  } TraditionalTriangle;

  // 16 uint8_ts
  typedef struct _compactTriangle 
  {
    float dotProduct; // precomputed dot (position*normal)
    signed short int normal[3];
    unsigned short int vertex[3];
  } CompactTriangle;

  inline bool intersectTriangle(Triangle &triangle, double orig[3], double dir[3], double *t, double *u, double *v)
  {
    double edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
    double det,inv_det;

    /* find vectors for two edges sharing vert0 */
    SUB(edge1, triangle.vertexB, triangle.vertexA);
    SUB(edge2, triangle.vertexC, triangle.vertexA);

    /* begin calculating determinant - also used to calculate U parameter */
    CROSS(pvec, dir, edge2);

    /* if determinant is near zero, ray lies in plane of triangle */
    det = DOT(edge1, pvec);

    if (det > -EPSILON && det < EPSILON)
      return 0;
    inv_det = 1.0 / det;

    /* calculate distance from vert0 to ray origin */
    SUB(tvec, orig, triangle.vertexA);

    /* calculate U parameter and test bounds */
    *u = DOT(tvec, pvec) * inv_det;
    if (*u < 0.0 || *u > 1.0)
      return 0;

    /* prepare to test V parameter */
    CROSS(qvec, tvec, edge1);

    /* calculate V parameter and test bounds */
    *v = DOT(dir, qvec) * inv_det;
    if (*v < 0.0 || *u + *v > 1.0)
      return 0;

    /* calculate t, ray intersects triangle */
    *t = DOT(edge2, qvec) * inv_det;
    return 1;
  }
}

#endif /* __TRIANGLE_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
