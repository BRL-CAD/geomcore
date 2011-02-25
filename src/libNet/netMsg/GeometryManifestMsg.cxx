/*         G E O M E T R Y M A N I F E S T M S G . C X X
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
/** @file GeometryManifestMsg.cxx
 *
 * Brief description
 *
 */

#include <arpa/inet.h>

#include <string>

#include "NetMsgTypes.h"
#include "GeometryManifestMsg.h"
#include "DataStream.h"

/* Normal Constructor */
GeometryManifestMsg::GeometryManifestMsg(std::list<std::string>& items) :
    NetMsg(GEOMETRYMANIFEST)
{
    this->itemData = new std::list<std::string> (items);
}

/* Reply Constructor */
GeometryManifestMsg::GeometryManifestMsg(NetMsg* msg, std::list<std::string>& items) :
	NetMsg(GEOMETRYMANIFEST, msg)
{
    this->itemData = new std::list<std::string> (items);
}

/* Deserializing Constructor */
GeometryManifestMsg::GeometryManifestMsg(DataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    this->itemData = new std::list<std::string> ();

    uint32_t numOfItems;
    numOfItems = ntohl(*ds->get(4));

    for (uint32_t i = 0; i < numOfItems; ++i) {
	std::string* tString = ds->getString();
	std::string newStr;
	newStr.append(*tString);
	this->itemData->push_back(newStr);
    }
}

/* Destructor */
GeometryManifestMsg::~GeometryManifestMsg()
{
    delete this->itemData;
}

bool GeometryManifestMsg::_serialize(DataStream* ds)
{
    uint32_t mt = htonl(itemData->size());
    ds->append((const char *)&mt, 4);

    for (std::list<std::string>::iterator it = this->itemData->begin(); it != this->itemData->end(); it++)
	ds->putString(*it);

    return true;
}

bool GeometryManifestMsg::_equals(const NetMsg& msg)
{
    GeometryManifestMsg& gmsg = (GeometryManifestMsg&) msg;
    std::list<std::string>::iterator it = this->itemData->begin(), git = gmsg.itemData->begin();

    for (; it != this->itemData->end(); it++, git++)
	if (*it != *git)
	    return false;
    return true;
}

std::string GeometryManifestMsg::toString()
{
    char buf[BUFSIZ];
    std::string out;

    snprintf(buf, BUFSIZ, "%s numberofItems: %d", NetMsg::toString().c_str(), (int)this->itemData->size());

    for (std::list<std::string>::iterator it=this->itemData->begin(); it != this->itemData->end(); it++) {
        snprintf(buf, BUFSIZ, "\n\t '%s'", it->c_str());
	out.append(buf);
    }

    out.append("\n");

    return out;
}

/*
 *Getters n Setters
 */
uint32_t GeometryManifestMsg::getNumOfItems()
{
    return this->itemData->size();
}

std::list<std::string>* GeometryManifestMsg::getItemData()
{
    return this->itemData;
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
