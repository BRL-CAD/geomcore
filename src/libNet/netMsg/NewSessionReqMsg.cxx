/*            N E W S E S S I O N R E Q M S G . C X X
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
/** @file NewSessionReqMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "NewSessionReqMsg.h"

/* Normal Constructor */
NewSessionReqMsg::NewSessionReqMsg(std::string uname, std::string passwd) :
    NetMsg(NEWSESSIONREQ), uname(uname), passwd(passwd)
{}

/* Reply Constructor */
NewSessionReqMsg::NewSessionReqMsg(NetMsg* msg, std::string uname, std::string passwd) :
	NetMsg(NEWSESSIONREQ, msg), uname(uname), passwd(passwd)
{}

/* Deserializing Constructor */
NewSessionReqMsg::NewSessionReqMsg(ByteBuffer* bb, Portal* origin) :
    NetMsg(bb, origin)
{
    this->uname = bb->getString();
    this->passwd = bb->getString();
}

/* Destructor */
NewSessionReqMsg::~NewSessionReqMsg()
{}

bool NewSessionReqMsg::_serialize(ByteBuffer* bb)
{
    bb->putString(this->uname);
    bb->putString(this->passwd);
    return true;
}

std::string NewSessionReqMsg::toString()
{
    char out[BUFSIZ];

    snprintf(out, BUFSIZ, "%s\t  uname: %s\t  passwd: %s", NetMsg::toString().c_str(), this->uname.c_str(), this->passwd.c_str());

    return std::string(out);
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
std::string NewSessionReqMsg::getUName()
{
    return this->uname;
}
std::string NewSessionReqMsg::getPasswd()
{
    return this->passwd;
}

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
