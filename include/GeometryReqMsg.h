/*                G E O M E T R Y R E Q M S G . H
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
/** @file GeometryReqMsg.h
 *
 * Brief description
 *
 */

#ifndef __GEOMETRYREQMSG_H__
#define __GEOMETRYREQMSG_H__

#include "GenericOneStringMsg.h"

class GeometryReqMsg : public GenericOneStringMsg
{
public:

	/* Only Constructor */
	GeometryReqMsg(std::string path, bool recurse);

	/* Reply Constructor */
	GeometryReqMsg(NetMsg* msg, std::string path, bool recurse);

	/* Deserializing Constructor */
	GeometryReqMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GeometryReqMsg();

	virtual std::string toString();

	/*
	 *Getters n Setters
	 */
	bool getRecurse();

	std::string getPath();

private:
	bool recurse;

	bool _serialize(DataStream* ds);
	bool _equals(const NetMsg& msg);

	/* Disable copy cstr and =operator */
	GeometryReqMsg(GeometryReqMsg const&):GenericOneStringMsg(0,""){};
	GeometryReqMsg& operator=(GeometryReqMsg const&){};
};

#endif /* __GEOMETRYREQMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
