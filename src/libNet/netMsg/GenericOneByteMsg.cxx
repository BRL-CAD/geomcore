/*             G E N E R I C O N E B Y T E M S G . C X X
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
/** @file GenericOneByteMsg.cxx
 *
 * Brief description
 *
 */

#include "GenericOneByteMsg.h"

/* Normal Constructor */
GenericOneByteMsg::GenericOneByteMsg(uint32_t type, uint8_t b) :
  NetMsg(type), data(b)
{}

/* Reply Constructor */
GenericOneByteMsg::GenericOneByteMsg(uint32_t type, NetMsg* msg, uint8_t b) :
    NetMsg(type, msg), data(b)
{}

/* Deserializing Constructor */
GenericOneByteMsg::GenericOneByteMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
  this->data = bb->get();
}

/* Destructor */
GenericOneByteMsg::~GenericOneByteMsg()
{}

bool
GenericOneByteMsg::_serialize(ByteBuffer* bb)
{
  bb->put(this->data);
  return true;
}

std::string
GenericOneByteMsg::toString()
{
  char buf[BUFSIZ];
  snprintf(buf, BUFSIZ, "%s\t data: '%d'", NetMsg::toString().c_str(),
      this->data);
  return std::string(buf);
}

bool
GenericOneByteMsg::_equals(const NetMsg& msg)
{
  GenericOneByteMsg& gmsg = (GenericOneByteMsg&) msg;
  return (this->getData() == gmsg.getData());
}

/*
 *Getters n Setters
 */
uint8_t
GenericOneByteMsg::getData()
{
  return this->data;
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
