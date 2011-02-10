/*           G E O M E T R Y M A N I F E S T M S G . H
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
/** @file GeometryManifestMsg.h
 *
 * Brief description
 *
 */

#ifndef __GEOMETRYMANIFESTMSG_H__
#define __GEOMETRYMANIFESTMSG_H__

#include "NetMsg.h"

#include <vector>

class GeometryManifestMsg : public NetMsg
{
public:

	/* Normal Constructor */
	GeometryManifestMsg(std::list<std::string>& items);

	/* Reply Constructor */
	GeometryManifestMsg(NetMsg* msg, std::list<std::string>& items);

	/* Deserializing Constructor */
	GeometryManifestMsg(QDataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GeometryManifestMsg();

	virtual std::string toString();

	/*
	 *Getters n Setters
	 */
	uint32_t getNumOfItems();
	std::list<std::string>* getItemData();

private:
	std::list<std::string>* itemData;

	bool _serialize(QDataStream* ds);
	bool _equals(const NetMsg& msg);

	/* Disable copy cstr and =operator */
	GeometryManifestMsg(GeometryManifestMsg const&):NetMsg(0){};
	GeometryManifestMsg& operator=(GeometryManifestMsg const&){};
};

#endif /* __GEOMETRYMANIFESTMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
