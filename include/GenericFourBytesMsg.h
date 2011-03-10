/*             G E N E R I C F O U R B Y T E S M S G . H
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
/** @file GenericFourBytesMsg.h
 *
 * Generic NetMsg Virtual Subclass containing a single Four Byte Field
 *
 */

#ifndef __GENERICFOURBYTESMSG_H__
#define __GENERICFOURBYTESMSG_H__

#include "NetMsg.h"

class GenericFourBytesMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericFourBytesMsg(uint32_t type, uint32_t b);

	/* Reply Constructor */
	GenericFourBytesMsg(uint32_t type, NetMsg* msg, uint32_t b);

	/* Deserializing Constructor */
	GenericFourBytesMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericFourBytesMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	uint32_t getData();
	uint32_t data;

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericFourBytesMsg(GenericFourBytesMsg const&):NetMsg(0){};
	GenericFourBytesMsg& operator=(GenericFourBytesMsg const&){};
};

#endif /* __GENERICFOURBYTESMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
