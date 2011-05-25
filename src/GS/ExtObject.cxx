/*                   E X T O B J E C T . C X X
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
/** @file ExtObject.cxx
 *
 */

#include "ExtObject.h"
#include <iostream>


ExtObject::ExtObject( std::string objName, bu_external* ext
) : ext(ext), objName(objName) {}

ExtObject::~ExtObject(void){
  delete ext;
}

void
ExtObject::serialize(ByteBuffer* bb)
{
  bb->put((char*)this->ext->ext_buf, this->ext->ext_nbytes);
}

std::string
ExtObject::getObjectName()
{
  return this->objName;
}

void
ExtObject::printObjState()
{
  std::cout << "ext*: " << ((this->ext == NULL) ? "NULL" : "Set") << "\n";
  std::cout << "filePath: " << filePath << "\n";
  std::cout << "objName: " << objName << std::endl;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
