/*        G E N E R I C E I G H T B Y T E S M S G . C X X
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
/** @file GenericEightBytesMsg.cxx
 *
 * Brief description
 *
 */


#include "GenericEightBytesMsg.h"
#include <sstream>

/* Normal Constructor */
GenericEightBytesMsg::GenericEightBytesMsg(uint32_t type, quint64 data) :
    NetMsg(type), data(data)
{}

/* Reply Constructor */
GenericEightBytesMsg::GenericEightBytesMsg(uint32_t type, NetMsg* msg, quint64 data) :
	NetMsg(type, msg), data(data)
{}

/* Deserializing Constructor */
GenericEightBytesMsg::GenericEightBytesMsg(QDataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    *ds >> this->data;
}

/* Destructor */
GenericEightBytesMsg::~GenericEightBytesMsg()
{}

bool GenericEightBytesMsg::_serialize(QDataStream* ds)
{
    *ds << this->data;
    return true;
}

std::string GenericEightBytesMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s\t data: '%d'", NetMsg::toString().c_str(), this->data);

    return std::string(out);
}

bool GenericEightBytesMsg::_equals(const NetMsg& msg)
{
    GenericEightBytesMsg& gmsg = (GenericEightBytesMsg&) msg;

    if (this->getData() != gmsg.getData()) {
	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
quint64 GenericEightBytesMsg::getData()
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
