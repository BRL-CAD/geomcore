/*             G E N E R I C T W O B Y T E S M S G . H
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
/** @file GenericTwoBytesMsg.h
 *
 * Generic NetMsg Virtual Subclass containing a single Two Byte Field
 *
 */

#ifndef __GENERICTWOBYTESMSG_H__
#define __GENERICTWOBYTESMSG_H__

#include "NetMsg.h"

class GenericTwoBytesMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericTwoBytesMsg(uint32_t type, uint16_t b);

	/* Reply Constructor */
	GenericTwoBytesMsg(uint32_t type, NetMsg* msg, uint16_t b);

	/* Deserializing Constructor */
	GenericTwoBytesMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericTwoBytesMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	uint16_t getData();
	uint16_t data;

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericTwoBytesMsg(GenericTwoBytesMsg const&):NetMsg(0){};
	GenericTwoBytesMsg& operator=(GenericTwoBytesMsg const&){};
};

#endif /* __GENERICTWOBYTESMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
