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
GenericMultiByteMsg::GenericMultiByteMsg(quint32 type, char* dataIn,
	quint32 dataInLen) :
    NetMsg(type), dataLen(dataInLen)
{
    /* Deep copy */
    this->data = (char*) malloc(dataInLen);

    for (quint32 i = 0; i < dataInLen; ++i) {
	this->data[i] = dataIn[i];
    }
}

/* Reply Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(quint32 type, NetMsg* msg, char* dataIn, quint32 dataInLen) :
	NetMsg(type, msg), dataLen(dataInLen)
{
    /* Deep copy */
    this->data = (char*) malloc(dataInLen);

    for (quint32 i = 0; i < dataInLen; ++i) {
	this->data[i] = dataIn[i];
    }
}

/* Deserializing Constructor */
GenericMultiByteMsg::GenericMultiByteMsg(QDataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    *ds >> this->dataLen;
    this->data = (char*) malloc(dataLen);

    for (quint32 i = 0; i < this->dataLen; ++i) {
	quint8 c;
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
    for (quint32 i = 0; i < this->dataLen; ++i) {

	/* Oddness, the DataStream won't detect this is a quint8 */
	/* Therefore you MUST cast it. */
	*ds << (quint8) this->data[i];
    }
    return true;
}

QString GenericMultiByteMsg::toString()
{
    QString out;

    out.append(NetMsg::toString());
    out.append("\t dataLen: '");
    out.append(QString::number(this->dataLen));
    out.append("'\t data: ");

    for (quint32 i = 0; i < this->dataLen; ++i) {
	out.append(QString::number(this->data[i]));
	out.append(", ");
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

    for (quint32 i = 0; i < gmsg.getDataLen(); ++i) {
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
quint32 GenericMultiByteMsg::getDataLen()
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
