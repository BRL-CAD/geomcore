/*             G E O M E T R Y S E R V I C E . C X X
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
/** @file GeometryService.cxx
 *
 */

#include "GeometryService.h"
#include "SessionManager.h"
#include "FileDataSource.h"
#include "PingMsg.h"
#include "PongMsg.h"

GeometryService::GeometryService(const std::string localNodeName,
                                 const std::string listenAddy,
                                 const uint16_t listenPort)
:   ControlledThread(localNodeName)
{
    this->dataMan = DataManager::getInstance();
    this->log = Logger::getInstance();
    this->log->logINFO("GeometryService", localNodeName + " is starting up...");

    this->portalMan = new PortalManager(localNodeName, listenPort, listenAddy);
    this->registerMsgRoutes();
}

GeometryService::~GeometryService()
{
    delete portalMan;
}

DataManager*
GeometryService::getDataManager()
{
    return this->dataMan;
}

void
GeometryService::registerMsgRoutes()
{
    NetMsgRouter* router = NetMsgRouter::getInstance();

    router->registerType(PING, this);
    router->registerType(PONG, this);

    router->registerType(NEWSESSIONREQ, SessionManager::getInstance());
    //router->registerType(SESSIONINFO, SessionManager::getInstance());
    router->registerType(DISCONNECTREQ, SessionManager::getInstance());

    router->registerType(DISCONNECTREQ, this->portalMan);

    router->registerType(CMD_SHUTDOWN, this);

    router->registerType(DIRLISTREQ, DataManager::getInstance());
    router->registerType(DIRLISTRES, DataManager::getInstance());

    router->registerType(GEOMETRYREQ, DataManager::getInstance());
    router->registerType(GEOMETRYCHUNK, DataManager::getInstance());
    router->registerType(GEOMETRYMANIFEST, DataManager::getInstance());
}

bool
GeometryService::preRunHook()
{
    //Do init stuff here
    this->log->logINFO("GeometryService", "Running");

    return true;
}

void
GeometryService::_run()
{
    this->log->logINFO("GeometryService", "Starting PortalManager");
    this->portalMan->start();

    while (this->getRunCmd() == true)
        usleep(50000);

    this->portalMan->shutdown(true);
}

bool
GeometryService::postRunHook()
{
    //Do teardown stuff here
    this->log->logINFO("GeometryService", "Shutdown");

    return true;
}

bool
GeometryService::handleNetMsg(NetMsg* msg)
{
    uint16_t type = msg->getMsgType();
    char buf[BUFSIZ];

    switch(type) {
	case CMD_SHUTDOWN:
	    log->logINFO("GeometryService", "Remote Shutdown Initiated.");
	    this->portalMan->shutdown();
	    this->shutdown();
	    return true;
	case FAILURE:
	    {
		FailureMsg* fMsg = (FailureMsg*)msg;
		uint8_t fc = fMsg->getFailureCode();

		GSUuid re = fMsg->getReUUID();

		snprintf(buf, BUFSIZ, "Recv'ed A FailureMsg with code: %d (%x)", fc, fc);
		log->logINFO("GeometryService", buf);
		return true;
	    }
	case PING:
	    {
		Portal* p = msg->getOrigin();
		PingMsg* pingMsg = (PingMsg*)msg;

		std::stringstream ss;

		std::string remNodeName("unknown");
		if (p != NULL)
		    remNodeName = p->getRemoteNodeName();

		ss << "PING from: '" << remNodeName << "' ";
		ss << "Start Time: " << pingMsg->getStartTime();

		log->logINFO("GeometryService", ss.str());

		if (p != NULL) {
		    PongMsg pongMsg((PingMsg*)msg);
		    p->send(&pongMsg);
		} else {
		    log->logINFO("GeometryService", "Can't return ping.  NULL Portal*");
		}

		return true;
	    }
	case PONG:
	    {
		Portal* p = msg->getOrigin();
		PongMsg* pongMsg = (PongMsg*)msg;

		/* calc current and differential times */
		uint64_t start = pongMsg->getStartTime();
		uint64_t now = Logger::getCurrentTime();
		uint64_t diff = now - start;

		std::string remNodeName("unknown");

		if (p != NULL)
		    remNodeName = p->getRemoteNodeName();

		std::stringstream ss;
		ss << "PONG from: '" << remNodeName << "' ";
		ss << " Start: " << start;
		ss << " Now: " << now;
		ss << " Diff: " << diff;

		log->logINFO("GSClient", ss.str());
		return true;
	    }
    }
    return false;
}

std::string
GeometryService::getLocalNodeName()
{
    return this->getThreadName();
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
