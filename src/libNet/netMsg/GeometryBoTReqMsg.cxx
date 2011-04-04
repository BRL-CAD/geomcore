/*           G E O M E T R Y B O T R E Q M S G . C X X
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

/** @file GeometryBoTReqMsg.cxx
 *
 * Request geometry in BoT format, tesselating if required. Each region
 * holds exactly 1 BoT primitive.
 *
 */

#include "NetMsgTypes.h"
#include "GeometryBoTReqMsg.h"

/* Normal Constructor */
GeometryBoTReqMsg::GeometryBoTReqMsg(std::string path, bool recurse) :
    GenericOneStringMsg(GEOMETRYREQ, path), recurse(recurse)
{}

/* Reply Constructor */
GeometryBoTReqMsg::GeometryBoTReqMsg(NetMsg* msg, std::string path, bool recurse) :
	GenericOneStringMsg(GEOMETRYREQ, msg, path), recurse(recurse)
{}

/* Deserializing Constructor */
GeometryBoTReqMsg::GeometryBoTReqMsg(DataStream* ds, Portal* origin) :
    GenericOneStringMsg(ds, origin)
{
    recurse = *ds->get(1);
}

/* Destructor */
GeometryBoTReqMsg::~GeometryBoTReqMsg()
{}

bool GeometryBoTReqMsg::_serialize(DataStream* ds)
{
    /* Call the super */
    GenericOneStringMsg::_serialize(ds);

    ds->append((const char *)&recurse, 1);
    return true;
}

std::string GeometryBoTReqMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s' recurse: '%d'", GenericOneStringMsg::toString().c_str(), this->recurse);

    return std::string(out);
}

bool GeometryBoTReqMsg::_equals(const NetMsg& msg)
{
    GeometryBoTReqMsg& gmsg = (GeometryBoTReqMsg&) msg;

    if (this->getRecurse() != gmsg.getRecurse()) {
    	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
bool GeometryBoTReqMsg::getRecurse()
{
    return this->recurse;
}

std::string
GeometryBoTReqMsg::getPath()
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
