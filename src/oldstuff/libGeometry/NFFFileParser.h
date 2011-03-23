/*                 N F F F I L E P A R S E R . H
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
/** @file NFFFileParser.h
 *
 * Brief description
 *
 */

#ifndef __NFFFILEPARSER_H__
#define __NFFFILEPARSER_H__

// system headers
#include <iostream>
#include <fstream>
#include <vector>

/* interface header */
#include "Geometry/Scene.h"
#include "Point.h"
#include "Vec.h"
#include "RGB.h"

namespace Geometry {
  class NFFFileParser
  {
  private:
    std::ifstream input;
    std::string a_line;
    std::vector<std::string>rt_tokens;

    void look_from_at(Point &p);
    void look_up(Vec &v);
    void look_angle(double &angle);
    void look_hither(double &hither);
    void look_res(unsigned int &x_res, unsigned int &y_res);
    
    void background_color (RGB &c);
    void pos_light_loc(Point &p, RGB &c);
    void obj_mater_prop(RGB &c, double &Kd, double &Ks, double &s, double &T, double &ir);
    void polygon_prim(Point &v1, Point &v2, Point &v3);
    
    void split(const std::string &str, std::vector<std::string> &tokens);      

  protected:
    NFFFileParser(std::string);
    ~NFFFileParser();

    Scene* parse(std::string filename);

  };
}

#endif  /* __NFFFILEPARSER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
