/*                         V E C . C X X
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
/** @file Vec.cxx
 *
 * Brief description
 *
 */

#include "Vec.h"
#include <math.h>

std::ostream&
operator<<(std::ostream& s, const Vec& vec)
{
    return s << vec.X << " " << vec.Y << " " << vec.Z << " ";
}

Vec::Vec() {
  X= 0;
  Y= 0;
  Z= 0;
}

Vec::Vec(double x, double y, double z) {
  X= x;
  Y= y;
  Z= z;
}

Vec::Vec(Point A, Point B) {
  X= A.X-B.X;
  Y= A.Y-B.Y;
  Z= A.Z-B.Z;
}

void Vec::setVec(double x, double y, double z) {
  X= x;
  Y= y;
  Z= z;
}

double Vec::dot(Vec v2) {
  return(X * v2.X + Y * v2.Y + Z * v2.Z);
}

void Vec::cross(Vec v1, Vec v2){
  X= v1.Y * v2.Z - v1.Z * v2.Y;
  Y= v1.Z * v2.X - v1.X * v2.Z;
  Z= v1.X * v2.Y - v1.Y * v2.X;
}

void Vec::unitize() {
  double	L;

  L= sqrt(X*X + Y*Y + Z*Z);
  X/= L;
  Y/= L;
  Z/= L;
}

void Vec::add(Vec v1) {
  X+= v1.X;
  Y+= v1.Y;
  Z+= v1.Z;
}

void Vec::sub(Vec v1) {
  X-= v1.X;
  Y-= v1.Y;
  Z-= v1.Z;
}

void Vec::mult(double s) {
  X*= s;
  Y*= s;
  Z*= s;
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
