/*                 D I R L I S T R E S M S G . H
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
/** @file DirListResMsg.h
 *
 * Brief description
 *
 */


#ifndef __DIRLISTRESMSG_H__
#define __DIRLISTRESMSG_H__

#include "NetMsg.h"
#include <list>

class DirListResMsg : public NetMsg
{
public:
	/* Normal Constructor */
	DirListResMsg(std::list<std::string>* items);

	/* Reply Constructor */
	DirListResMsg(NetMsg* msg, std::list<std::string>* items);

	/* Deserializing Constructor */
	DirListResMsg(ByteBuffer* bb, Portal* origin);

	/* Destructor */
	virtual ~DirListResMsg();

	virtual std::string toString();

	/*
	 *Getters n Setters
	 */
	uint32_t getNumOfItems();
	uint32_t getItems(std::list<std::string>* items);

private:
	std::list<std::string>* itemData;

	bool _serialize(ByteBuffer* bb);
	bool _equals(const NetMsg& msg);

	/* Disable copy cstr and =operator */
	DirListResMsg(DirListResMsg const&):NetMsg(0){};
	DirListResMsg& operator=(DirListResMsg const&){};
};

#endif /* __DIRLISTRESMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
