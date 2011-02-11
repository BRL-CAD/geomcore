/*            N E W S E S S I O N R E Q M S G . H
 * BRLCAD
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
/** @file NewSessionReqMsg.h
 *
 * Brief description
 *
 */

#ifndef __NEWSESSIONREQMSG_H__
#define __NEWSESSIONREQMSG_H__

#include "NetMsg.h"

class NewSessionReqMsg : public NetMsg
{
public:
	/* Normal Constructor */
	NewSessionReqMsg(std::string uname, std::string passwd);

	/* Reply Constructor */
	NewSessionReqMsg(NetMsg* msg, std::string uname, std::string passwd);

	/* Deserializing Constructor */
	NewSessionReqMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~NewSessionReqMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();
	std::string getUName();
	std::string getPasswd();

protected:
	std::string uname;
	std::string passwd;

	virtual bool _serialize(DataStream* ds);
	virtual bool _equals(const NetMsg& msg);


private:
	/* Disable copy cstr and =operator */
	NewSessionReqMsg(NewSessionReqMsg const&):NetMsg(0){};
	NewSessionReqMsg& operator=(NewSessionReqMsg const&){};

};

#endif /* __NEWSESSIONREQMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
