/*                         P O I N T . H
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
/** @file Point.h
 *
 * Brief description
 *
 */

#ifndef __POINT_H__
#define __POINT_H__

#include <iostream>
#include "Vec.h"

using std::cout;
using std::endl;

/**
 *  Stores a point consisting of x,y, and z components; has methods to get x,y,z values,
 *  to set x,y,z values and to add or subtract two points componentwise.
 */
class Point
{
 public:
  /** Creates a point at (0,0,0) */
  Point();
  /** Creates a point at (x,y,z) */
  Point(double x, double y, double z);

  inline double getZ() {
    return Z;
  }

  inline double getY() {
    return Y;
  }

  inline double getX() {
    return X;
  }

#if 0
  /** gets x value */
  double getX();
  /** gets y value */
  double getY();
  /** gets z value */
  double getZ();
#endif
  /** sets x,y,z values */
  void setPt(double x, double y, double z);
  /** adds two points to return a vector */
  Vec add(Point &p, Point &q);
  /** subtracts two points to return a vector */
  Vec sub(Point &p, Point &q);
  /** does a brl-cad style join1 */
  Point join1(Point &pt, double dist, Vec &dir);

  friend std::ostream&
    operator<<(std::ostream&, const Point&);

  /** the point consisting of three components */
  //    double p[3];
  double X;
  double Y;
  double Z;
};
#else
class Point;
#endif //__POINT_H__

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
