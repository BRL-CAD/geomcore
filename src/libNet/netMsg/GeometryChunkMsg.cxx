/*            G E O M E T R Y C H U N K M S G . C X X
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
/** @file GeometryChunkMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "GeometryChunkMsg.h"
#include <sstream>

/* Normal Constructor */
GeometryChunkMsg::GeometryChunkMsg(std::string path, char* dataIn, uint32_t dataInLen) :
    GenericMultiByteMsg(GEOMETRYCHUNK, dataIn, dataInLen), path(path)
{}

/* Reply Constructor */
GeometryChunkMsg::GeometryChunkMsg(NetMsg* msg, std::string path, char* dataIn, uint32_t dataInLen) :
	GenericMultiByteMsg(GEOMETRYCHUNK, msg, dataIn, dataInLen), path(path)
{}

/* Deserializing Constructor */
GeometryChunkMsg::GeometryChunkMsg(DataStream* ds, Portal* origin) :
    GenericMultiByteMsg(ds, origin)
{
	this->path = ds->getString()->c_str();
}

/* Destructor */
GeometryChunkMsg::~GeometryChunkMsg()
{}

std::string
GeometryChunkMsg::getPath()
{
	return this->path;
}


bool
GeometryChunkMsg::_serialize(DataStream* ds)
{
	GenericMultiByteMsg::_serialize(ds);
    ds->putString(this->path);
    return true;
}

std::string
GeometryChunkMsg::toString()
{
    char buf[BUFSIZ];
    std::string out;

    snprintf(buf, BUFSIZ, "%s\t dataLen: '%d'\t data: ", NetMsg::toString().c_str(), this->dataLen);
    out.assign(buf);

    for (uint32_t i = 0; i < this->dataLen; ++i) {
        snprintf(buf, BUFSIZ, "%d, ", this->data[i]);
	out.append(buf);
    }

    out.append(" Path: ");
    out.append(this->path);

    return out;
}

bool
GeometryChunkMsg::_equals(const NetMsg& msg)
{
	GeometryChunkMsg& gmsg = (GeometryChunkMsg&) msg;

    if (this->getDataLen() != gmsg.getDataLen())
	    return false;

    for (uint32_t i = 0; i < gmsg.getDataLen(); ++i)
		if (this->getData()[i] != gmsg.getData()[i])
			return false;

    if (this->path != gmsg.path)
    	return false;

    return true;
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
