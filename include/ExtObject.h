/*                     E X T O B J E C T . H
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
/** @file ExtObject.h
 *
 * Brief description
 *
 */

#ifndef __EXTOBJECT_H___
#define __EXTOBJECT_H___

#include "ByteBuffer.h"
#include "GeometryChunkMsg.h"

#include <bu.h>
#include <string>

class ExtObject {
public:
  /* Normal cstr */
  ExtObject(std::string objName, bu_external* ext);

  /* factory method */
  static ExtObject* makeExtObject(ByteBuffer* data);

  virtual  ~ExtObject(void);

  void serialize(ByteBuffer* bb);
  ByteBuffer* serialize();

  std::string  getFullPath();

  GeometryChunkMsg* toGeometryChunkMsg(NetMsg* reply = 0);

  void  printObjState();

private:
  bu_external* ext;
  std::string fullPath;
};

#endif /* __EXTOBJECT_H___ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
