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

#include <arpa/inet.h>

#include "GenericMultiByteMsg.h"
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <string.h>

/* Normal Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, ByteArray* dataIn) :
    NetMsg(type)
{
    /* Deep copy */
	this->data = new ByteArray(*dataIn);
}

/* Reply Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, NetMsg* msg, ByteArray* dataIn) :
	NetMsg(type, msg)
{
    /* Deep copy */
	this->data = new ByteArray(*dataIn);
}

/* Deserializing Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(DataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    int dataLen = htonl(*(uint32_t*)ds->get(sizeof(uint32_t)));
	this->data = new ByteArray(ds->get(dataLen), dataLen);
}

/* Destructor */
GenericMultiByteMsg::~GenericMultiByteMsg()
{
    free(this->data);
}

bool GenericMultiByteMsg::_serialize(DataStream* ds)
{
    uint32_t len = htonl(this->data->size());
    ds->append((const char *)&len, 4);
	std::cout << "Ping\n";
    ds->append((const char *)this->data->data(), this->data->size());
	std::cout << "Ping\n";
	return true;
}

std::string GenericMultiByteMsg::toString()
{
    char buf[BUFSIZ];
    std::string out;

    snprintf(buf, BUFSIZ, "%s\t dataLen: '%d'\t data: ", NetMsg::toString().c_str(), this->data->size());
    out.assign(buf);

    for (uint32_t i = 0; i < this->data->size(); ++i) {
        snprintf(buf, BUFSIZ, "%d, ", this->data->data()[i]);
	out.append(buf);
    }

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
    return this->data->data();
}
uint32_t GenericMultiByteMsg::getDataLen()
{
    return this->data->size();
}

ByteArray*
GenericMultiByteMsg::getByteArray()
{
	return new ByteArray(*this->data);
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
