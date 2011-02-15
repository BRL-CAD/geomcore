/*                S E S S I O N M A N A G E R . H
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
/** @file SessionManager.h
 *
 * Provides management functions for active Sessions.
 *
 */

#ifndef __SESSIONMANAGER_H__
#define __SESSIONMANAGER_H__

#include "Logger.h"
#include "Session.h"
#include "INetMsgHandler.h"
#include "NewSessionReqMsg.h"
#include "TypeOnlyMsg.h"

#include <list>
#include <map>
#include <GSThread.h>

class SessionManager : public INetMsgHandler
{
public:
    static SessionManager* getInstance();
    virtual ~SessionManager();
    bool handleNetMsg(NetMsg* msg);

    Session* getSession(Account* a);
    Session* getSession(GSUuid* sessID);
    Session* getSession(Portal* p);

private:
    static SessionManager* pInstance;
    SessionManager();

    Logger* log;

    GSMutex listLock;
    std::list<Session*> sessionList;

    Session* newSession(Account* a);
    void putCache(Session* s);
    void remCache(Session* s);

    void handleNewSessionReqMsg(NewSessionReqMsg* msg);
    void handleDisconnectReqMsg(TypeOnlyMsg* msg);

	/* Disable copy cstr and =operator */
	SessionManager(SessionManager const&){};
	SessionManager& operator=(SessionManager const&){};
};

#endif /* __SESSIONMANAGER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
