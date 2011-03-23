/*                         R G B . C X X
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
/** @file RGB.cxx
 *
 * Brief description
 *
 */

#include "RGB.h"

RGB::RGB(){
    val[0]=0;
    val[1]=0;
    val[2]=0;
////  cout << "   RGB::RGB()" << endl;
}
RGB:: RGB(double r, double g, double b){
    val[0]=r;
    val[1]=g;
    val[2]=b;
////    cout << "   RGB::RGB(r,g,b)" << endl; 
}
double RGB::getBlue(){
  //returns blue value
  ////cout << "RGB::getBlue" << endl;
  return val[2];
}

double RGB::getGreen(){
 //returns green value
 //// cout << "RGB::getGreen" << endl;
  return val[1];
}

double RGB::getRed(){
 //returns red value
 //// cout << "RGB::getRed" << endl;
  return val[0];
}

void RGB::setColor(double r, double g, double b){
  //sets RGB colors
    val[0]=r;
    val[1]=g;
    val[2]=b;    
 //// cout << "RGB::setColor" << endl;
}

void RGB::add(RGB & p, RGB & q){
        val[0]= p.val[0] + q.val[0];
        val[1]= p.val[1] + q.val[1];
        val[2]= p.val[2] + q.val[2];
        
// adds two RGBs componentwise and stores in current object
}

void RGB::scale(double k, RGB & p  ){
    
    val[0] = k*p.val[0];
    val[1] = k*p.val[1];
    val[2] = k*p.val[2];

//scales an RGB using componentwise multiplication with a scalar.
}

std::ostream&
operator<<(std::ostream& s, const RGB& rgb)
{
    return s << rgb.val[0] << " " << rgb.val[1] << " " << rgb.val[2] << " ";
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
