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

/* Normal Constructor */
GeometryManifestMsg::GeometryManifestMsg(QList<QString>& items) :
    NetMsg(GEOMETRYMANIFEST)
{
    this->itemData = new QList<QString> ();
    this->itemData->append(items);
}

/* Reply Constructor */
GeometryManifestMsg::GeometryManifestMsg(NetMsg* msg, QList<QString>& items) :
	NetMsg(GEOMETRYMANIFEST, msg)
{
    this->itemData = new QList<QString> ();
    this->itemData->append(items);
}

/* Deserializing Constructor */
GeometryManifestMsg::GeometryManifestMsg(QDataStream* ds, Portal* origin) :
    NetMsg(ds, origin)
{
    this->itemData = new QList<QString> ();

    quint32 numOfItems;
    *ds >> numOfItems;

    for (quint32 i = 0; i < numOfItems; ++i) {
	QString* tString = DataStreamUtils::getString(ds);
	QString newStr;
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

    for (quint32 i = 0; i < this->itemData->size(); ++i) {
	DataStreamUtils::putString(ds, this->itemData->at(i));
    }

    return true;
}

bool GeometryManifestMsg::_equals(const NetMsg& msg)
{
    GeometryManifestMsg& gmsg = (GeometryManifestMsg&) msg;

    for (quint32 i = 0; i < this->itemData->size(); ++i) {
	if (this->itemData->at(i) != gmsg.itemData->at(i)) {
	    return false;
	}
    }
    return true;
}

QString GeometryManifestMsg::toString()
{
    QString out;

    out.append(NetMsg::toString());
    out.append(" numberofItems: ");
    out.append(QString::number(this->itemData->size()));

    for (quint32 i = 0; i < this->itemData->size(); ++i) {
	out.append("\n\t '");
	out.append(this->itemData->at(i));
	out.append("'");
    }

    out.append("\n");

    return out;
}

/*
 *Getters n Setters
 */
quint32 GeometryManifestMsg::getNumOfItems()
{
    return this->itemData->size();
}

QList<QString>* GeometryManifestMsg::getItemData()
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
