/*               N F F F I L E P A R S E R . C X X
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
/** @file NFFFileParser.cxx
 *
 * Brief description
 *
 */

#include "NFFFileParser.h"

using namespace Geometry;

#include <stdio.h>
#include <stdlib.h>
#include "Geometry/Triangle.h"
#include "Geometry/View.h"

Scene* NFFFileParser::parse(std::string filename) 
{
  Scene *scene = new Scene();
  std::vector<Triangle*> geometry;
  std::vector<View*> views;

  //  cout << "READER::openFile(" << filename << ")" <<endl;

  // attempt to open file
  // alt flow 4: if unsuccessful , print "cannot read input file" on 
  // stderr output, and exit with -1 exit condition.
  // else store filename as filename attribute (input filename) and return (0)
  if (filename.size() == 0) {
    cout << "ERROR: file name is empty" << endl;
    return scene;
  }
  
  input.open(filename.c_str(), std::ios::in);
  
  if (!input) {
    cout << "ERROR: cannot read input file" << endl;
    return scene;
  }
  
  //
  // We appear to have success;
  //
  char first_letter, second_letter;
  long line_number = 0;

  //
  // Camera Stuff - necessary because the of the camera interface.
  //

  Point r_from;
  Point r_at;
  Vec r_up(0.0, 0.0, 1.0);
  double r_angle = 30.0;
  double r_hither = 0.0;
  unsigned int r_xres = 512, r_yres = 512;
  
  RGB b_color(0.0, 0.0, 0.0);
  
  // Light Properties
  RGB l_color(1.0, 1.0, 1.0);
  Point l_point;
    
  // Material Properties
  RGB m_color(0.5, 0.5, 0.5);
  double m_Kd = 1.0, m_Ks = 0.0, m_s = 0.0, m_T = 0.0, m_ir = 0.0;
  
  Point v_p1, v_p2, v_p3;

  while (!input.eof()) {
     line_number++;
     getline (input, a_line);
        
     first_letter = a_line[0];
     second_letter = a_line[1];
     
     switch (first_letter) {
     case 'v':
        // Do Nothing!
        break;
     case 'a':
        if (second_letter == 't') {
           look_from_at(r_at);
        } else {
           look_angle(r_angle);
        }
        break;
     case 'u':
        look_up(r_up);
        break;
     case 'h':
        look_hither(r_hither);
        break;
     case 'r':
        look_res(r_xres, r_yres);
        break;
     case 'b':
        background_color(b_color);
        break;
     case 'l':
        pos_light_loc(l_point, l_color);
	//        sp.light(l_point, l_color);
        break;
     case 'f':
        if (second_letter == 'r') {
           look_from_at(r_from);
        } else {
           obj_mater_prop(m_color, m_Kd, m_Ks, m_s, m_T, m_ir);
	   //           sp.material(m_color, m_Kd, m_Ks, m_s, m_T, m_ir);
        }
        break;
     case 'p':
       {
	 polygon_prim(v_p1, v_p2, v_p3);
	 Triangle *t = new Triangle();
	 t->vertexA[0] = v_p1.X;
	 t->vertexA[1] = v_p1.Y;
	 t->vertexA[2] = v_p1.Z;
	 t->vertexB[0] = v_p2.X;
	 t->vertexB[1] = v_p2.Y;
	 t->vertexB[2] = v_p2.Z;
	 t->vertexC[0] = v_p3.X;
	 t->vertexC[1] = v_p3.Y;
	 t->vertexC[2] = v_p3.Z;
	 //	t.normal[0] = t.normal[1] = t.normal[2] = 0.0;

	 geometry.push_back(t);
	 //        sp.triangle(v_p1, v_p2, v_p3);
	 break;
       }
     case '#':
        // This is a comment -- do nothing!
        break;
     default:
        if (!a_line.empty()) {
           cout << "Line #" << line_number << " is not valid:" << endl
                << "[ " << a_line << " ]" << endl;
           exit(-1);
        }
        break;
     }
     
     rt_tokens.clear();
     
  }
  
  input.close();

  double from[3];
  double at[3];
  double up[3];

  from[0] = r_from.X;
  from[1] = r_from.Y;
  from[2] = r_from.Z;
  at[0] = r_at.X;
  at[1] = r_at.Y;
  at[2] = r_at.Z;
  up[0] = r_up.X;
  up[1] = r_up.Y;
  up[2] = r_up.Z;

  View *view = new View(from, at, up, r_angle, r_hither);
  view->setResolution(r_xres, r_yres);
  views.push_back(view);
  
  //  cam.set(r_from, r_at,  r_up, r_angle, r_hither,  r_xres, r_yres);
  //  cam.background(b_color);

  scene->add(geometry);
  scene->add(views);

  return scene;
}

void NFFFileParser::look_from_at(Point &p) {
   split(a_line, rt_tokens);
   
   p.setPt(atof(rt_tokens[1].c_str()),
           atof(rt_tokens[2].c_str()),
           atof(rt_tokens[3].c_str())
           );
}

void NFFFileParser::look_up(Vec &v) {
   split(a_line, rt_tokens);
   
   v.setVec(atof(rt_tokens[1].c_str()),
           atof(rt_tokens[2].c_str()),
           atof(rt_tokens[3].c_str())
           );
}

void NFFFileParser::look_angle(double &angle) {
   split(a_line, rt_tokens);
   
   angle = atof(rt_tokens[1].c_str());
}

void NFFFileParser::look_hither(double &hither) {
   split(a_line, rt_tokens);
   
   hither = atof(rt_tokens[1].c_str());
}

void NFFFileParser::look_res(unsigned int &x_res, unsigned int &y_res) {
   split(a_line, rt_tokens);
   
   x_res = atoi(rt_tokens[1].c_str());
   y_res = atoi(rt_tokens[2].c_str());
}

void NFFFileParser:: background_color (RGB &c) {   
   split(a_line, rt_tokens);
   
   c.setColor(atof(rt_tokens[1].c_str()),
              atof(rt_tokens[2].c_str()),
              atof(rt_tokens[3].c_str())
              );
}

void NFFFileParser::pos_light_loc(Point &p, RGB &c) {
   
   split(a_line, rt_tokens);
   int ntokens = rt_tokens.size();
   
   p.setPt(atof(rt_tokens[1].c_str()),
           atof(rt_tokens[2].c_str()),
           atof(rt_tokens[3].c_str())
           );
   
   if (ntokens == 7) {
      c.setColor(atof(rt_tokens[4].c_str()),
                 atof(rt_tokens[5].c_str()),
                 atof(rt_tokens[6].c_str())
                 );
   } else {
      c.setColor(1.0, 1.0, 1.0);
   }
}

void NFFFileParser::obj_mater_prop(RGB &c, double &Kd, double &Ks, double &s, double &T, double &ir) {
   // default values
   T = 0.0;
   ir = 0.0;

   split(a_line, rt_tokens);
   int ntokens = rt_tokens.size();
   
   if (ntokens >= 4) {
      c.setColor(atof(rt_tokens[1].c_str()),
                 atof(rt_tokens[2].c_str()),
                 atof(rt_tokens[3].c_str())
                 );
   }
   
   if (ntokens >= 6) {
      Ks = atof(rt_tokens[5].c_str());
      Kd = atof(rt_tokens[4].c_str());
   } else {
      Kd = atof(rt_tokens[4].c_str());
      Ks = 1 - Kd;
   }
   
   if (ntokens >= 7) {
      s = atof(rt_tokens[6].c_str());
   }
   
   if (ntokens >= 8) {
      T = atof(rt_tokens[7].c_str());
   }
   
   if (ntokens == 9) {
      ir = atof(rt_tokens[8].c_str());
   }
}

void NFFFileParser::polygon_prim(Point &v1, Point &v2, Point &v3) {
   int p_size;
   double x_v, y_v, z_v;
   
   split(a_line, rt_tokens);
   
   p_size = atoi(rt_tokens[1].c_str());
   
   if ((p_size < 3) || (p_size > 3)) {
      cout << "Polygon with less or more than 3 sides are not support!" << endl;
   } else {
      rt_tokens.clear();
      getline(input, a_line);
      split(a_line, rt_tokens);
      
      x_v = atof(rt_tokens[0].c_str());
      y_v = atof(rt_tokens[1].c_str());
      z_v = atof(rt_tokens[2].c_str());
      
      v1.setPt(x_v, y_v, z_v);
      
      rt_tokens.clear();
      getline(input, a_line);
      split(a_line, rt_tokens);
      
      x_v = atof(rt_tokens[0].c_str());
      y_v = atof(rt_tokens[1].c_str());
      z_v = atof(rt_tokens[2].c_str());
      
      v2.setPt(x_v, y_v, z_v);      
      
      rt_tokens.clear();
      getline(input, a_line);
      split(a_line, rt_tokens);
      
      x_v = atof(rt_tokens[0].c_str());
      y_v = atof(rt_tokens[1].c_str());
      z_v = atof(rt_tokens[2].c_str());
      
      v3.setPt(x_v, y_v, z_v);
   }
}

void NFFFileParser::split(const std::string &str, std::vector<std::string> &tokens) {
   const std::string DELIMITERS = " ";

   // Skip delimiters at beginning.
   std::string::size_type lastPos = str.find_first_not_of(DELIMITERS, 0);

   // Find first "non-delimiter".
   std::string::size_type pos     = str.find_first_of(DELIMITERS, lastPos);

   while (std::string::npos != pos || std::string::npos != lastPos) {
       // Found a token, add it to the vector.
       tokens.push_back(str.substr(lastPos, pos - lastPos));

       // Skip delimiters.  Note the "not_of"
       lastPos = str.find_first_not_of(DELIMITERS, pos);

       // Find next "non-delimiter"
       pos = str.find_first_of(DELIMITERS, lastPos);
   }
}

NFFFileParser::NFFFileParser(std::string filename)
{}

NFFFileParser::~NFFFileParser()
{}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
