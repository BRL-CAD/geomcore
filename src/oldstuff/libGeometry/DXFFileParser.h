/*                 D X F F I L E P A R S E R . H
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
/** @file DXFFileParser.h
 *
 * Brief description
 *
 */

#ifndef __DXFFILEPARSER_H__
#define __DXFFILEPARSER_H__

// system headers
#include <iostream>
#include <vector>

/* interface headers */
#include "Geometry/Scene.h"

namespace Geometry {
  class DXFFileParser
  {
  private:

    typedef struct {
      double	x;
      double	y;
      double	z;
    } ADM_3;

    void nextLine(char *B, FILE *fh);

  protected:
    DXFFileParser(std::string);
    ~DXFFileParser();

    Scene* parse(std::string filename);
  };
}

#endif  /* __DXFFILEPARSER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
