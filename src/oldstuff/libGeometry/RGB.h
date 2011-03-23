/*                           R G B . H
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
/** @file RGB.h
 *
 * Brief description
 *
 */

#ifndef __RGB_H__
#define __RGB_H__
#include <iostream>
//using std::cout;
//using std::endl;
/** Implements methods to manipulate colors */
class RGB {
public:
 friend std::ostream&
     operator<<(std::ostream&, const RGB&);

    RGB();
    RGB(double r, double g, double b);
    /** Sets red, green, and blue values */
    void setColor(double r, double g, double b);
    /** Returns red value */
    double getRed();
    /** Returns green value */
    double getGreen();
    /** Returns blue value */
    double getBlue();
    /** adds two RGBs componentwise */
    void add(RGB & p, RGB & q);
    /** scales an RGB using componentwise multiplication with a scalar */
    void scale(double k , RGB & p );
    
private:    
    /** Stores the red-green-blue values */
    double val[3];
};

#endif //__RGB_H__

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
