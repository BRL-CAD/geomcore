/*                        V I E W . C X X
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
/** @file View.cxx
 *
 * Brief description
 *
 */

/* interface header */
#include "Geometry/View.h"

using namespace Geometry;

View::View(double from[3], double at[3], double up[3], double angle, double hither)
{
  _from[0] = from[0];
  _from[1] = from[1];
  _from[2] = from[2];
  _at[0] = at[0];
  _at[1] = at[1];
  _at[2] = at[2];
  _up[0] = up[0];
  _up[1] = up[1];
  _up[2] = up[2];
  _angle = angle;
  _hither = hither;
}

View::~View()
{
  _from[0] = 0.0;
  _from[1] = 0.0;
  _from[2] = 0.0;
  _at[0] = 0.0;
  _at[1] = 0.0;
  _at[2] = 0.0;
  _up[0] = 0.0;
  _up[1] = 0.0;
  _up[2] = 0.0;
  _angle = 0.0;
  _hither = 0.0;
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
