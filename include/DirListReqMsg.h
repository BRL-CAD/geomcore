/*                 D I R L I S T R E Q M S G . H
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
/** @file DirListReqMsg.h
 *
 * Brief description
 *
 */

#ifndef __DIRLISTREQMSG_H__
#define __DIRLISTREQMSG_H__

#include "NetMsg.h"

class DirListReqMsg : public NetMsg
{
public:
	/* Only Constructor */
	DirListReqMsg(std::string path);

	/* Reply Constructor */
	DirListReqMsg(NetMsg* msg, std::string path);

	/* Deserializing Constructor */
	DirListReqMsg(ByteBuffer* bb, Portal* origin);

	/* Destructor */
	virtual ~DirListReqMsg();

	/*
	 *Getters n Setters
	 */
	std::string getPath();
	virtual std::string toString();

private:
	std::string path;

	bool _serialize(ByteBuffer* bb);
	bool _equals(const NetMsg& msg);

	/* Disable copy cstr and =operator */
	DirListReqMsg(DirListReqMsg const&):NetMsg(0){};
	DirListReqMsg& operator=(DirListReqMsg const&){};
};

#endif /* __DIRLISTREQMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
