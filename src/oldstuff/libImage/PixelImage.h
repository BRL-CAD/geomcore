/*                    P I X E L I M A G E . H
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
/** @file PixelImage.h
 *
 * Brief description
 *
 */

#ifndef __PIXELIMAGE_H__
#define __PIXELIMAGE_H__

#include <string>

namespace Image 
{
  class PixelImage
  {
  protected:
    unsigned long int _width;
    unsigned long int _height;

    double _backgroundColor;
    
  public:
    PixelImage(unsigned long int width=1, unsigned long int height=1);
    ~PixelImage();

    inline unsigned long int getWidth() const;
    inline unsigned long int getHeight() const;

    void readFromFile(std::string filename);

  };

  inline unsigned long int PixelImage::getWidth() const
  {
    return _width;
  }

  inline unsigned long int PixelImage::getHeight() const
  {
    return _height;
  }
}

#endif /* __PIXELIMAGE_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
