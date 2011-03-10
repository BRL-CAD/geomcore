/*                        N E T M S G . H
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
/** @file NetMsg.h
 *
 * Brief description
 *
 */

#ifndef __NETMSG_H__
#define __NETMSG_H__

#include <sstream>
#include <string>
#include <stdint.h>
#include <stdio.h>

#include "DataStream.h"
#include "GSUuid.h"

#include "ByteArray.h"

class Portal;

class NetMsg
{
public:
	/* Normal Constructor */
	NetMsg(uint16_t mType);

	/* Reply Constructor */
	NetMsg(uint16_t mType, NetMsg* msg);

	/* Deserializing Constructor */
	NetMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~NetMsg();

	/* Serializers */
	ByteArray* serialize();
	void serialize(ByteArray* ba);

	/*
	 *Getters n Setters
	 */
	uint16_t getMsgType() const;
	GSUuid *getMsgUUID() const;
	bool msgHasReUUID() const;
	GSUuid *getReUUID() const;
	Portal* getOrigin() const;

	/*
	 * Utilities
	 */
	virtual std::string toString();
	virtual std::string toStdString();
	virtual bool equals(const NetMsg& other);
	void printMe();

	bool operator== (const NetMsg& other);

protected:
	uint16_t msgType;
	GSUuid *msgUUID;
	bool hasReUUID;
	GSUuid *reUUID;
	Portal* origin;

	virtual bool _serialize(DataStream* ds) = 0;
	virtual bool _equals(const NetMsg& msg) = 0;

private:
	/* Disable Default Constructor */
	NetMsg(){};

	/* Disable copy cstr and =operator */
	NetMsg(NetMsg const&){};
	NetMsg& operator=(NetMsg const&){};
};

#endif /* __NETMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
