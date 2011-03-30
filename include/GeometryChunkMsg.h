/*              G E O M E T R Y C H U N K M S G . H
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
/** @file GeometryChunkMsg.h
 *
 * Brief description
 *
 */

#ifndef __GEOMETRYCHUNKMSG_H__
#define __GEOMETRYCHUNKMSG_H__

#include "GenericMultiByteMsg.h"

class GeometryChunkMsg : public GenericMultiByteMsg
{
public:
	/* Normal Constructor */
	GeometryChunkMsg(std::string path, ByteArray* dataIn);

	/* Reply Constructor */
	GeometryChunkMsg(NetMsg* msg, std::string path, ByteArray* dataIn);

	/* Deserializing Constructor */
	GeometryChunkMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GeometryChunkMsg();

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);

	std::string toString();
	std::string getPath();

private:
	std::string path;

	/* Disable copy cstr and =operator */
	GeometryChunkMsg(GeometryChunkMsg const&):GenericMultiByteMsg(-1,NULL){};
	GeometryChunkMsg& operator=(GeometryChunkMsg const&){};
};

#endif /* __GEOMETRYCHUNKMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
