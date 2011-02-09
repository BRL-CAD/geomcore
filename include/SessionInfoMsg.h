/*            S E S S I O N I N F O M S G . H
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
/** @file SessionInfoMsg.h
 *
 * Brief description
 *
 */

#ifndef __SESSIONINFOMSG_H__
#define __SESSIONINFOMSG_H__

#include "NetMsg.h"

class SessionInfoMsg : public NetMsg
{
public:
	/* Normal Constructor */
	SessionInfoMsg(QUuid sessionID);

	/* Reply Constructor */
	SessionInfoMsg(NetMsg* msg, QUuid sessionID);

	/* Deserializing Constructor */
	SessionInfoMsg(QDataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~SessionInfoMsg();

	/*
	 * Utilities
	 */
	virtual std::string toString();
	QUuid getSessionID();

protected:
	QUuid sessionID;

	bool _serialize(QDataStream* ds);
	bool _equals(const NetMsg& msg);

private:
	/* Disable copy cstr and =operator */
	SessionInfoMsg(SessionInfoMsg const&):NetMsg(0){};
	SessionInfoMsg& operator=(SessionInfoMsg const&){};
};

#endif /* __SESSIONINFOMSG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
