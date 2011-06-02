/*            G E O M E T R Y C H U N K M S G . C X X
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
/** @file GeometryChunkMsg.cxx
 *
 * Brief description
 *
 */

#include "GeometryChunkMsg.h"
#include <raytrace.h>

/* Normal Constructor */
GeometryChunkMsg::GeometryChunkMsg(std::string path, ByteBuffer* rawDataIn) :
  NetMsg(GEOMETRYCHUNK), path(path)
{
  /* Deep copy */
  this->data = rawDataIn->duplicate();
}

/* Reply Constructor */
GeometryChunkMsg::GeometryChunkMsg(NetMsg* msg, std::string path, ByteBuffer* rawDataIn) :
    NetMsg(GEOMETRYCHUNK, msg), path(path)
{
  /* Deep copy */
  this->data = rawDataIn->duplicate();
}

/* Deserializing Constructor */
GeometryChunkMsg::GeometryChunkMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
  /* Path */
  this->path = bb->getString();

  /* then Data */
  int len = bb->get32bit();
  this->data = ByteBuffer::allocate(len + 1);
  bb->get(this->data->array(), len);
}

/* Destructor */
GeometryChunkMsg::~GeometryChunkMsg()
{
  free(this->data);
}

std::string
GeometryChunkMsg::getPath()
{
  return this->path;
}

bool
GeometryChunkMsg::_serialize(ByteBuffer* bb)
{
  /* Path */
  bb->putString(this->path);

  /* then Data */
  uint32_t len = 0;
  if (this->data->position() != 0) {
    this->data->flip();
    len = this->data->limit();
  } else {
    len = this->data->position();
  }
  bb->put32bit(len);
  bb->put(this->data->array(), len);

  return true;
}

std::string
GeometryChunkMsg::toString()
{
  std::string out = NetMsg::toString();
  out.append(" Path: ");
  out.append(this->path);
  out += "\tData: " + this->data->toString();
  return out;
}

bool
GeometryChunkMsg::_equals(const NetMsg& msg)
{
  GeometryChunkMsg& gmsg = (GeometryChunkMsg&) msg;

  if (this->getDataLen() != gmsg.getDataLen())
    return false;

  for (uint32_t i = 0; i < gmsg.getDataLen(); ++i)
    if (this->data->array()[i] != gmsg.data->array()[i])
      return false;

  if (this->path != gmsg.path)
    return false;

  return true;
}

/*
 *Getters n Setters
 */

uint32_t GeometryChunkMsg::getDataLen()
{
  return this->data->position();
}

ByteBuffer*
GeometryChunkMsg::getByteBuffer()
{
  return new ByteBuffer(*this->data);
}


/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
