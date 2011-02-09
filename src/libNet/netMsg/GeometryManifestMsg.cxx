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

#include "NetMsgTypes.h"
#include "GeometryManifestMsg.h"
#include "DataStreamUtils.h"

#include <string>

/* Normal Constructor */
GeometryManifestMsg::GeometryManifestMsg(QList<std::string>& items) :
    NetMsg(GEOMETRYMANIFEST)
{
    this->itemData = new QList<std::string> ();
    this->itemData->append(items);
}

/* Reply Constructor */
GeometryManifestMsg::GeometryManifestMsg(NetMsg* msg, QList<std::string>& items) :
	NetMsg(GEOMETRYMANIFEST, msg)
{
    this->itemData = new QList<std::string> ();
    this->itemData->append(items);
}

/* Deserializing Constructor */
GeometryManifestMsg::GeometryManifestMsg(QDataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    this->itemData = new QList<std::string> ();

    uint32_t numOfItems;
    *ds >> numOfItems;

    for (uint32_t i = 0; i < numOfItems; ++i) {
	std::string* tString = DataStreamUtils::getString(ds);
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

bool GeometryManifestMsg::_serialize(QDataStream* ds)
{
    *ds << this->itemData->size();

    for (uint32_t i = 0; i < this->itemData->size(); ++i) {
	DataStreamUtils::putString(ds, this->itemData->at(i));
    }

    return true;
}

bool GeometryManifestMsg::_equals(const NetMsg& msg)
{
    GeometryManifestMsg& gmsg = (GeometryManifestMsg&) msg;

    for (uint32_t i = 0; i < this->itemData->size(); ++i) {
	if (this->itemData->at(i) != gmsg.itemData->at(i)) {
	    return false;
	}
    }
    return true;
}

std::string GeometryManifestMsg::toString()
{
    char buf[BUFSIZ];
    std::string out;

    snprintf(buf, BUFSIZ, "%s numberofItems: %d", NetMsg::toString().c_str(), this->itemData->size());

    for (uint32_t i = 0; i < this->itemData->size(); ++i) {
        snprintf(buf, BUFSIZ, "\n\t '%s'", this->itemData->at(i).c_str());
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

QList<std::string>* GeometryManifestMsg::getItemData()
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
