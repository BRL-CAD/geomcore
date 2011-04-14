/*             G E N E R I C T W O B Y T E S M S G . C X X
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
/** @file GenericTwoBytesMsg.cxx
 *
 * Brief description
 *
 */

#include "GenericTwoBytesMsg.h"

/* Normal Constructor */
GenericTwoBytesMsg::GenericTwoBytesMsg(uint32_t type, uint16_t b) :
    NetMsg(type), data(b)
{}

/* Reply Constructor */
GenericTwoBytesMsg::GenericTwoBytesMsg(uint32_t type, NetMsg* msg, uint16_t b) :
     NetMsg(type, msg), data(b)
{}

/* Deserializing Constructor */
GenericTwoBytesMsg::GenericTwoBytesMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
    this->data = bb->get16bit();
}

/* Destructor */
GenericTwoBytesMsg::~GenericTwoBytesMsg()
{}

bool
GenericTwoBytesMsg::_serialize(ByteBuffer* bb)
{
  bb->put16bit(this->data);
  return true;
}

std::string
GenericTwoBytesMsg::toString()
{
  char buf[BUFSIZ];
  snprintf(buf, BUFSIZ, "%s\t data: '%d'", NetMsg::toString().c_str(),
      this->data);
  return std::string(buf);
}

bool
GenericTwoBytesMsg::_equals(const NetMsg& msg)
{
  GenericTwoBytesMsg& gmsg = (GenericTwoBytesMsg&) msg;

  if (this->getData() != gmsg.getData())
    return false;
  return true;
}

/*
 *Getters n Setters
 */
uint16_t
GenericTwoBytesMsg::getData()
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
