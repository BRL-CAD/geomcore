/*                         T I M E R . H
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
/** @file Timer.h
 *
 * Brief description
 *
 */

#ifndef __TIMER_H__
#define __TIMER_H__

#include <ctime>

namespace Utility {
  /** Low-resolution (hundredth of a second) timer class
   */
  class Timer
  {
  protected:
    bool _running;
    bool _started;
    clock_t _start;
    double _accumulated;

    double elapsedSinceStart();

  public:
    Timer();
    Timer(const Timer& timer);
    ~Timer();

    void start();
    void stop();
    void reset();

    double elapsed();

    bool running();
  };
}

#endif

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
