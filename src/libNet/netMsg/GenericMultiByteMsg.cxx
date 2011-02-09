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

/* Normal Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, char* dataIn,
	uint32_t dataInLen) :
    NetMsg(type), dataLen(dataInLen)
{
    /* Deep copy */
    this->data = (char*) malloc(dataInLen);

    for (uint32_t i = 0; i < dataInLen; ++i) {
	this->data[i] = dataIn[i];
    }
}

/* Reply Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(uint32_t type, NetMsg* msg, char* dataIn, uint32_t dataInLen) :
	NetMsg(type, msg), dataLen(dataInLen)
{
    /* Deep copy */
    this->data = (char*) malloc(dataInLen);

    for (uint32_t i = 0; i < dataInLen; ++i) {
	this->data[i] = dataIn[i];
    }
}

/* Deserializing Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(QDataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    *ds >> this->dataLen;
    this->data = (char*) malloc(dataLen);

    for (uint32_t i = 0; i < this->dataLen; ++i) {
	uint8_t c;
	*ds >> c;
	this->data[i] = c;
    }
}

/* Destructor */
GenericMultiByteMsg::~GenericMultiByteMsg()
{
    free(this->data);
}

bool GenericMultiByteMsg::_serialize(QDataStream* ds)
{
    *ds << this->dataLen;
    for (uint32_t i = 0; i < this->dataLen; ++i) {

	/* Oddness, the DataStream won't detect this is a uint8_t */
	/* Therefore you MUST cast it. */
	*ds << (uint8_t) this->data[i];
    }
    return true;
}

std::string GenericMultiByteMsg::toString()
{
    char buf[BUFSIZ];
    std::string out;

    snprintf(buf, BUFSIZ, "%s\t dataLen: '%d'\t data: ", NetMsg::toString().c_str(), this->dataLen);
    out.assign(buf);

    for (uint32_t i = 0; i < this->dataLen; ++i) {
        snprintf(buf, BUFSIZ, "%d, ", this->data[i]);
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
    return this->data;
}
uint32_t GenericMultiByteMsg::getDataLen()
{
    return this->dataLen;
}

QByteArray*
GenericMultiByteMsg::getQByteArray()
{
	QByteArray* data = new QByteArray(this->data, this->dataLen);
	return data;
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
