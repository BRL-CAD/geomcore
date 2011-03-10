/*          G E N E R I C E I G H T B Y T E S M S G . H
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
/** @file GenericEightBytesMsg.h
 *
 * Brief description
 *
 */

#ifndef __GENERICEIGHTBYTESMSG_H__
#define __GENERICEIGHTBYTESMSG_H__

#include "NetMsg.h"

class GenericEightBytesMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericEightBytesMsg(uint32_t type, uint64_t b);

	/* Reply Constructor */
	GenericEightBytesMsg(uint32_t type, NetMsg* msg, uint64_t b);

	/* Deserializing Constructor */
	GenericEightBytesMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericEightBytesMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	uint64_t getData();
	uint64_t data;

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericEightBytesMsg(GenericEightBytesMsg const&):NetMsg(0){};
	GenericEightBytesMsg& operator=(GenericEightBytesMsg const&){};
};

#endif /* __GENERICEIGHTBYTESMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
