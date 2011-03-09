/*             G E N E R I C M U L T I B Y T E M S G . H
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
/** @file GenericMultiByteMsg.h
 *
 * Generic NetMsg Virtual Subclass containing a 32 bit Length and a variable length Byte[] Field
 *
 */

#ifndef __GENERICMULTIBYTEMSG_H__
#define __GENERICMULTIBYTEMSG_H__

#include "NetMsg.h"
#include "ByteArray.h"

class GenericMultiByteMsg : public NetMsg
{
public:
	/* Normal Constructor */
	GenericMultiByteMsg(uint32_t type, char* dataIn, uint32_t dataInLen);

	/* Reply Constructor */
	GenericMultiByteMsg(uint32_t type, NetMsg* msg, char* dataIn, uint32_t dataInLen);

	/* Deserializing Constructor */
	GenericMultiByteMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~GenericMultiByteMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();
	ByteArray* getByteArray();

protected:
	uint32_t getDataLen();
	char* getData();

	uint32_t dataLen;
	char* data;

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	GenericMultiByteMsg(GenericMultiByteMsg const&):NetMsg(0){};
	GenericMultiByteMsg& operator=(GenericMultiByteMsg const&){};
};

#endif /* __GENERICMULTIBYTEMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
