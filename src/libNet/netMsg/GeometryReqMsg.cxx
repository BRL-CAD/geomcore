/*              G E O M E T R Y R E Q M S G . C X X
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
/** @file GeometryReqMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "GeometryReqMsg.h"

/* Normal Constructor */
GeometryReqMsg::GeometryReqMsg(uint8_t requestType, std::string data) :
    GenericOneStringMsg(GEOMETRYREQ, data), reqType(requestType)
{}

/* Reply Constructor */
GeometryReqMsg::GeometryReqMsg(NetMsg* msg, uint8_t requestType, std::string data) :
	GenericOneStringMsg(GEOMETRYREQ, msg, data), reqType(requestType)
{}

/* Deserializing Constructor */
GeometryReqMsg::GeometryReqMsg(QDataStream* ds, Portal* origin) :
    GenericOneStringMsg(ds, origin)
{
    *ds >> this->reqType;
}

/* Destructor */
GeometryReqMsg::~GeometryReqMsg()
{}

bool GeometryReqMsg::_serialize(QDataStream* ds)
{
    /* Call the super */
    GenericOneStringMsg::_serialize(ds);

    *ds << this->reqType;
    return true;
}

std::string GeometryReqMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s' requestType: '%d'", GenericOneStringMsg::toString().c_str(), this->reqType);

    return std::string(out);
}

bool GeometryReqMsg::_equals(const NetMsg& msg)
{
    GeometryReqMsg& gmsg = (GeometryReqMsg&) msg;

    if (this->getRequestType() != gmsg.getRequestType()) {
	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
uint8_t GeometryReqMsg::getRequestType()
{
    return this->reqType;
}

std::string
GeometryReqMsg::getData()
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
