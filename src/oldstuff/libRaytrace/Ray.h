/*                           R A Y . H
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
/** @file Ray.h
 *
 * Brief description
 *
 */

#ifndef __RAY_H__
#define __RAY_H__

#include <iostream>

#include "Ray.h"

namespace Geometry {
  class Ray
  {
  private:
    double _position[3];
    double _direction[3];
    
  public:
    Ray(double position[3], double direction[3]);
    ~Ray();
  };

  Ray::Ray(double position[3], double direction[3])
  {
    _position[0] = position[0];
    _position[1] = position[1];
    _position[2] = position[2];

    _direction[0] = direction[0];
    _direction[1] = direction[0];
    _direction[0] = direction[0];
  }
}

#endif  /* __RAY_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
