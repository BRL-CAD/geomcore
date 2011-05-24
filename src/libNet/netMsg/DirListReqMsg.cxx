/*               D I R L I S T R E Q M S G . C X X
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
/** @file DirListReqMsg.cxx
 *
 * Brief description
 *
 */

#include "DirListReqMsg.h"

/* Normal Constructor */
DirListReqMsg::DirListReqMsg(std::string path) :
  NetMsg(DIRLISTREQ), path(path)
{}

/* Reply Constructor */
DirListReqMsg::DirListReqMsg(NetMsg* msg, std::string path) :
  NetMsg(DIRLISTREQ, msg), path(path)
{}

/* Deserializing Constructor */
DirListReqMsg::DirListReqMsg(ByteBuffer* bb, Portal* origin) :
 NetMsg(bb,origin)
{
  this->path = bb->getString();
}

/* Destructor */
DirListReqMsg::~DirListReqMsg()
{}

bool
DirListReqMsg::_serialize(ByteBuffer* bb)
{
  bb->putString(this->path);
  return true;
}

std::string
DirListReqMsg::toString()
{
  std::string out;
  out += "Path: '" + this->path + "'";
  return out;
}

bool
DirListReqMsg::_equals(const NetMsg& msg)
{
  DirListReqMsg& gmsg = (DirListReqMsg&) msg;
  if (this->path != gmsg.path)
    return false;
  return true;
}

/*
 *Getters n Setters
 */

std::string
DirListReqMsg::getPath()
{
  return this->path;
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
