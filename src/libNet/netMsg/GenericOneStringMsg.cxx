/*             G E N E R I C O N E S T R I N G M S G . C X X
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
/** @file GenericOneStringMsg.cxx
 *
 * Brief description
 *
 */

#include "GenericOneStringMsg.h"

/* Normal Constructor */
GenericOneStringMsg::GenericOneStringMsg(uint32_t type, std::string s) :
    NetMsg(type), strData(s)
{}

/* Reply Constructor */
GenericOneStringMsg::GenericOneStringMsg(uint32_t type, NetMsg* msg, std::string s) :
     NetMsg(type, msg), strData(s)
{}

/* Deserializing Constructor */
GenericOneStringMsg::GenericOneStringMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
  this->strData = bb->getString();
}

/* Destructor */
GenericOneStringMsg::~GenericOneStringMsg()
{}

bool
GenericOneStringMsg::_serialize(ByteBuffer* bb)
{
  bb->putString(this->strData);
  return true;
}

std::string
GenericOneStringMsg::toString()
{
  std::string out;

  out.append(NetMsg::toString());
  out.append("\t  strData: '");
  out.append(this->strData + "'");

  return out;
}

bool
GenericOneStringMsg::_equals(const NetMsg& msg)
{
  GenericOneStringMsg& gmsg = (GenericOneStringMsg&) msg;

  if (this->getStrData() != gmsg.getStrData())
    return false;
  return true;
}

/*
 *Getters n Setters
 */
std::string
GenericOneStringMsg::getStrData()
{
  return this->strData;
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
