/*          R E M O T E N O D E N A M E S E T M S G . H
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
/** @file RemoteNodenameSetMsg.h
 *
 * Brief description
 *
 */

#ifndef __REMOTENODENAMESETMSG_H__
#define __REMOTENODENAMESETMSG_H__

#include "GenericOneStringMsg.h"

class RemoteNodenameSetMsg : public GenericOneStringMsg
{
public:
	/* Normal Constructor */
	RemoteNodenameSetMsg(std::string s);

	/* Reply Constructor */
	RemoteNodenameSetMsg(NetMsg* msg, std::string gsNodename);

	/* Deserializing Constructor */
	RemoteNodenameSetMsg(QDataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~RemoteNodenameSetMsg();

	std::string getRemoteNodename();

private:
	/* Disable copy cstr and =operator */
	RemoteNodenameSetMsg(RemoteNodenameSetMsg const&):GenericOneStringMsg(0, ""){};
	RemoteNodenameSetMsg& operator=(RemoteNodenameSetMsg const&){};
};

#endif /* __REMOTENODENAMESETMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
