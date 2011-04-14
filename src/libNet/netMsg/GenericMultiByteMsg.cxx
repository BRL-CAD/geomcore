/*             G E N E R I C M U L T I B Y T E M S G . C X X
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
/** @file GenericMultiByteMsg.cxx
 *
 * Brief description
 *
 */

#include "GenericMultiByteMsg.h"
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <string.h>

/* Normal Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, ByteBuffer* dataIn) :
    NetMsg(type)
{
    /* Deep copy */
	this->data = dataIn->duplicate();
}

/* Reply Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, NetMsg* msg, ByteBuffer* dataIn) :
	NetMsg(type, msg)
{
    /* Deep copy */
	this->data = dataIn->duplicate();
}

/* Deserializing Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
    int len = bb->get32bit();
    this->data = ByteBuffer::allocate(len + 1);

    bb->get(this->data->array(), len);
}

/* Destructor */
GenericMultiByteMsg::~GenericMultiByteMsg()
{
    free(this->data);
}

bool GenericMultiByteMsg::_serialize(ByteBuffer* bb)
{
  uint32_t len = 0;

  if (this->data->position() != 0) {
    this->data->flip();
    len = this->data->limit();
  } else {
    len = this->data->position();
  }

  bb->put(this->data->array(), len);
  return true;
}

std::string GenericMultiByteMsg::toString()
{
    std::string out = NetMsg::toString();
    out += "\t" + this->data->toString();
    return out;
}

bool GenericMultiByteMsg::_equals(const NetMsg& msg)
{
    GenericMultiByteMsg& gmsg = (GenericMultiByteMsg&) msg;

    if (this->getDataLen() != gmsg.getDataLen()) {
	    std::cout << "\n1\n";
	return false;
    }

    for (uint32_t i = 0; i < gmsg.getDataLen(); ++i) {
	if (this->getData()[i] != gmsg.getData()[i]) {
	    return false;
	}
    }

    return true;
}

/*
 *Getters n Setters
 */
char* GenericMultiByteMsg::getData()
{
    return this->data->array();
}
uint32_t GenericMultiByteMsg::getDataLen()
{
    return this->data->position();
}

ByteBuffer*
GenericMultiByteMsg::getByteBuffer()
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
