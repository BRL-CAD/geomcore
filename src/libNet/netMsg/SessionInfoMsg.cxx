/*            S E S S I O N I N F O M S G . C X X
 * BRLCAD
 *
 * Copyright (c) 2010 United States Government as represented by
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
/** @file SessionInfoMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "SessionInfoMsg.h"
#include <sstream>

/* Normal Constructor */
SessionInfoMsg::SessionInfoMsg(QUuid sessionID) :
    NetMsg(SESSIONINFO), sessionID(sessionID)
{
}

/* Reply Constructor */
SessionInfoMsg::SessionInfoMsg(NetMsg* msg, QUuid sessionID) :
	NetMsg(SESSIONINFO, msg), sessionID(sessionID)
{
}

/* Deserializing Constructor */
SessionInfoMsg::SessionInfoMsg(QDataStream* ds, QString origin) :
    NetMsg(ds, origin)
{
    this->sessionID = *Utils::getQUuid(ds);
}

/* Destructor */
SessionInfoMsg::~SessionInfoMsg()
{
}

bool SessionInfoMsg::_serialize(QDataStream* ds)
{
    Utils::putQUuid(ds, this->sessionID);
    return true;
}

QString SessionInfoMsg::toString()
{
    QString out;

    out.append(NetMsg::toString());
    out.append("\t  SessionID: ");
    out.append(this->sessionID.toString());

    return out;
}

bool SessionInfoMsg::_equals(const NetMsg& msg)
{
    SessionInfoMsg& gmsg = (SessionInfoMsg&) msg;

    if (this->getSessionID() != gmsg.getSessionID()) {
	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
QUuid SessionInfoMsg::getSessionID()
{
    return this->sessionID;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
