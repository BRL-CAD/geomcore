#ifndef __NFFFILEPARSER_H__
#define __NFFFILEPARSER_H__

// system headers
#include <iostream>
#include <fstream>
#include <vector>

// interface headers
#include "Geometry/Scene.h"
#include "Point.h"
#include "Vec.h"
#include "RGB.h"


namespace Geometry {

  class NFFFileParser {

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

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
