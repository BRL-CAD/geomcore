/*             G E N E R I C F O U R B Y T E S M S G . C X X
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
/** @file GenericFourBytesMsg.cxx
 *
 * Brief description
 *
 */

#include "GenericFourBytesMsg.h"
#include <sstream>

/* Normal Constructor */
GenericFourBytesMsg::GenericFourBytesMsg(uint32_t type, uint32_t b) :
    NetMsg(type), data(b)
{}

/* Reply Constructor */
GenericFourBytesMsg::GenericFourBytesMsg(uint32_t type, NetMsg* msg, uint32_t b) :
	NetMsg(type, msg), data(b)
{}

/* Deserializing Constructor */
GenericFourBytesMsg::GenericFourBytesMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
    data = bb->get32bit();
}

/* Destructor */
GenericFourBytesMsg::~GenericFourBytesMsg()
{}

bool GenericFourBytesMsg::_serialize(ByteBuffer* bb)
{
  bb->put32bit(this->data);
  return true;
}

std::string GenericFourBytesMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s\t data: '%d'", NetMsg::toString().c_str(), this->data);

    return std::string(out);
}

bool GenericFourBytesMsg::_equals(const NetMsg& msg)
{
    GenericFourBytesMsg& gmsg = (GenericFourBytesMsg&) msg;

    if (this->getData() != gmsg.getData()) {
	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
uint32_t GenericFourBytesMsg::getData()
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
