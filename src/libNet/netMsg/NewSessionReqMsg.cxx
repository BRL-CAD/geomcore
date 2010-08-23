/*            N E W S E S S I O N R E Q M S G . C X X
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
/** @file NewSessionReqMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "NewSessionReqMsg.h"
#include <sstream>

/* Normal Constructor */
NewSessionReqMsg::NewSessionReqMsg(QString uname, QString passwd) :
    NetMsg(NEWSESSIONREQ), uname(uname), passwd(passwd)
{
}

/* Reply Constructor */
NewSessionReqMsg::NewSessionReqMsg(NetMsg* msg, QString uname, QString passwd) :
	NetMsg(NEWSESSIONREQ, msg), uname(uname), passwd(passwd)
{
}

/* Deserializing Constructor */
NewSessionReqMsg::NewSessionReqMsg(QDataStream* ds, QString origin) :
    NetMsg(ds, origin)
{
    this->uname = *Utils::getString(ds);
    this->passwd = *Utils::getString(ds);
}

/* Destructor */
NewSessionReqMsg::~NewSessionReqMsg()
{
}

bool NewSessionReqMsg::_serialize(QDataStream* ds)
{
    Utils::putString(ds, this->uname);
    Utils::putString(ds, this->passwd);
    return true;
}

QString NewSessionReqMsg::toString()
{
    QString out;

    out.append(NetMsg::toString());
    out.append("\t  uname: ");
    out.append(this->uname);
    out.append("\t  passwd: ");
    out.append(this->passwd);

    return out;
}

bool NewSessionReqMsg::_equals(const NetMsg& msg)
{
    NewSessionReqMsg& gmsg = (NewSessionReqMsg&) msg;

    if (this->uname != gmsg.uname) {
	return false;
    }
    if (this->passwd != gmsg.passwd) {
	return false;
    }

    return true;
}

/*
 *Getters n Setters
 */
QString NewSessionReqMsg::getUName()
{
    return this->uname;
}
QString NewSessionReqMsg::getPasswd()
{
    return this->passwd;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
