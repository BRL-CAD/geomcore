/*                       P O I N T . C X X
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
/** @file Point.cxx
 *
 * Brief description
 *
 */

#include "Point.h"

std::ostream&
operator<<(std::ostream& s, const Point& p)
{
    return s << p.X << " " << p.Y << " " << p.Z << " ";
}

Point::Point() {
  X = Y = Z = 0.0;
}

Point::Point(double x, double y, double z) {
	X = x;
  Y = y;
  Z = z;
}

Vec Point::sub(Point &p, Point &q) {
  Vec	V;

  V.setVec(p.X - q.X, p.Y - q.Y, p.Z - q.Z);
  return V;
}

Vec Point::add(Point &p, Point &q) {
  Vec	V;

  V.setVec(p.X + q.X, p.Y + q.Y, p.Z + q.Z);
  return V;
}

void Point::setPt(double x, double y, double z) {
  X = x;
  Y = y;
  Z = z;
}

Point Point::join1(Point &pt, double dist, Vec &dir) {
  Point		T;

  T.setPt(pt.X + dist*dir.X, pt.Y + dist*dir.Y, pt.Z + dist*dir.Z);
  return(T);
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
