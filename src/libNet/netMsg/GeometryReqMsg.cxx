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
GeometryReqMsg::GeometryReqMsg(std::string data, bool recurse) :
    GenericOneStringMsg(GEOMETRYREQ, data), recurse(recurse)
{}

/* Reply Constructor */
GeometryReqMsg::GeometryReqMsg(NetMsg* msg, std::string data, bool recurse) :
	GenericOneStringMsg(GEOMETRYREQ, msg, data), recurse(recurse)
{}

/* Deserializing Constructor */
GeometryReqMsg::GeometryReqMsg(DataStream* ds, Portal* origin) :
    GenericOneStringMsg(ds, origin)
{
    recurse = *ds->get(1);
}

/* Destructor */
GeometryReqMsg::~GeometryReqMsg()
{}

bool GeometryReqMsg::_serialize(DataStream* ds)
{
    /* Call the super */
    GenericOneStringMsg::_serialize(ds);

    ds->append((const char *)&recurse, 1);
    return true;
}

std::string GeometryReqMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s' recurse: '%d'", GenericOneStringMsg::toString().c_str(), this->recurse);

    return std::string(out);
}

bool GeometryReqMsg::_equals(const NetMsg& msg)
{
    GeometryReqMsg& gmsg = (GeometryReqMsg&) msg;

    if (this->getRecurse() != gmsg.getRecurse()) {
    	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
bool GeometryReqMsg::getRecurse()
{
    return this->recurse;
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
