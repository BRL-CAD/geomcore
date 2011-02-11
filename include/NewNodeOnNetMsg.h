/*               N E W N O D E O N N E T M S G . H
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
/** @file NewNodeOnNetMsg.h
 *
 * Brief description
 *
 */

#ifndef __NEWNODEONNETMSG_H__
#define __NEWNODEONNETMSG_H__

#include "GenericOneStringMsg.h"

class NewNodeOnNetMsg : public GenericOneStringMsg
{
public:
	/* Normal Constructor */
	NewNodeOnNetMsg(std::string s);

	/* Reply Constructor */
	NewNodeOnNetMsg(NetMsg* msg, std::string gsNodename);

	/* Deserializing Constructor */
	NewNodeOnNetMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~NewNodeOnNetMsg();

	std::string getNewNodename();

private:
	/* Disable copy cstr and =operator */
	NewNodeOnNetMsg(NewNodeOnNetMsg const&):GenericOneStringMsg(0,""){};
	NewNodeOnNetMsg& operator=(NewNodeOnNetMsg const&){};
};

#endif /* __NEWNODEONNETMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
