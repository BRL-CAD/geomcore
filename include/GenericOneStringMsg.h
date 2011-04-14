/*             G E N E R I C O N E S T R I N G M S G . H
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
/** @file GenericOneStringMsg.h
 *
 * Brief description
 *
 */

#ifndef __GENERICONESTRINGMSG_H__
#define __GENERICONESTRINGMSG_H__

#include "NetMsg.h"

class GenericOneStringMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericOneStringMsg(uint32_t type, std::string s);

	/* Reply Constructor */
	GenericOneStringMsg(uint32_t type, NetMsg* msg, std::string s);

	/* Deserializing Constructor */
	GenericOneStringMsg(ByteBuffer* bb, Portal* origin);

	/* Destructor */
	virtual ~GenericOneStringMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	std::string getStrData();
	std::string strData;

	virtual bool _serialize(ByteBuffer* bb);
	virtual bool _equals(const NetMsg& msg);

	/* Disable copy cstr and =operator */
	GenericOneStringMsg(GenericOneStringMsg const&):NetMsg(0){};
	GenericOneStringMsg& operator=(GenericOneStringMsg const&){};
};

#endif /* __GENERICONESTRINGMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
