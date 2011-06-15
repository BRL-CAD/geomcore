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


ExtObject::ExtObject( std::string fullPath, bu_external* ext
) : ext(ext), fullPath(fullPath) {}

ExtObject::~ExtObject(void)
{
  bu_free(ext, "ExtObject dstr");
}

ExtObject*
ExtObject::makeExtObject(ByteBuffer* bb)
{
  bu_external* ext = (bu_external*)bu_malloc(sizeof(bu_external), "ExtObject cstr");

  std::cout << "Before BU_EXTERNAL_INIT\n";
  BU_EXTERNAL_INIT(ext);
  std::cout << "After BU_EXTERNAL_INIT\n";

  /* Deserialize */

  /* path */
  std::string fullPath = bb->getString();
  if (fullPath.length() <= 0) return NULL;

  /* Magic */
  ext->ext_magic = bb->get32bit();

  /* Buf len */
  ext->ext_nbytes = bb->get32bit();
  if (ext->ext_nbytes <= 0) return NULL;

  /* Buf */
  ext->ext_buf = (uint8_t*)bu_malloc(ext->ext_nbytes, "ExtObject cstr");
  bb->get((char*)ext->ext_buf, ext->ext_nbytes);

  return new ExtObject(fullPath, ext);
}

void
ExtObject::serialize(ByteBuffer* bb)
{
  bb->putString(this->fullPath);
  bb->put32bit(this->ext->ext_magic);
  bb->put32bit(this->ext->ext_nbytes);
  bb->put((char*)this->ext->ext_buf, this->ext->ext_nbytes);
}

ByteBuffer*
ExtObject::serialize()
{
  int size = this->fullPath.length() + sizeof(int) + this->ext->ext_nbytes;
  ByteBuffer* bb = ByteBuffer::allocate(size);
  this->serialize(bb);
  return bb;
}

std::string
ExtObject::getFullPath()
{
  return this->fullPath;
}

void
ExtObject::printObjState()
{
  std::cout << "ext*: " << ((this->ext == NULL) ? "NULL" : "Set") << "\n";
  std::cout << "fullPath: " << fullPath << std::endl;
}

GeometryChunkMsg*
ExtObject::toGeometryChunkMsg(NetMsg* reply)
{
  GeometryChunkMsg* chunk = NULL;
  ByteBuffer* bb = this->serialize();

  if (reply == NULL)
    chunk = new GeometryChunkMsg(this->fullPath, bb);
  else
    chunk = new GeometryChunkMsg(reply, this->fullPath, bb);

  delete bb;
  return chunk;
}


// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
