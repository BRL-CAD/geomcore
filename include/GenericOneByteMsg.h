/*             G E N E R I C O N E B Y T E M S G . H
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
/** @file GenericOneByteMsg.h
 *
 *  Generic NetMsg Virtual Subclass containing a single One Byte Field 
 *
 */

#ifndef __GENERICONEBYTEMSG_H__
#define __GENERICONEBYTEMSG_H__

#include "NetMsg.h"

class GenericOneByteMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericOneByteMsg(uint32_t type, uint8_t b);

	/* Reply Constructor */
	GenericOneByteMsg(uint32_t type, NetMsg* msg, uint8_t b);

	/* Deserializing Constructor */
	GenericOneByteMsg(ByteBuffer* bb, Portal* origin);

	/* Destructor */
	virtual ~GenericOneByteMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();

protected:
	uint8_t getData();
	uint8_t data;

	virtual bool _serialize(ByteBuffer* bb);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericOneByteMsg(GenericOneByteMsg const&): NetMsg(0){};
	GenericOneByteMsg& operator=(GenericOneByteMsg const&){};
};

#endif /* __GENERICONEBYTEMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
