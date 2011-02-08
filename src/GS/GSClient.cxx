/*                    G S C L I E N T . C X X
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
/** @file GSClient.cxx
 * GSClient.cxx
 *
 */

#include "GSClient.h"
#include "Portal.h"
#include "NetMsgRouter.h"
#include "SessionInfoMsg.h"
#include "FailureMsg.h"
#include "PongMsg.h"

GSClient::GSClient(QString localNodeName) {
	this->log = Logger::getInstance();
	this->jobMan = JobManager::getInstance();
	this->jobMan->startup();

	this->portMan = new PortalManager(localNodeName);
	this->portMan->start();
	GSThread::msleep(100);

	this->registerMsgRoutes();
}

GSClient::~GSClient() {
	delete this->portMan;
}

void
GSClient::registerMsgRoutes()
{
	NetMsgRouter* router = NetMsgRouter::getInstance();

	router->registerType(DISCONNECTREQ, this->portMan);
	router->registerType(SESSIONINFO, this);
	router->registerType(FAILURE, this);
	router->registerType(PING, this);
	router->registerType(PONG, this);
}

bool
GSClient::handleNetMsg(NetMsg* msg)
{
	quint16 type = msg->getMsgType();
	switch(type) {
	case SESSIONINFO:
		{
			QString data =((SessionInfoMsg*)msg)->toString();
			log->logINFO("GSClient", "Recv'ed SessionInfo: " + data);
			return true;
		}
	case FAILURE:
		{
			FailureMsg* fMsg = (FailureMsg*)msg;
			quint8 fc = fMsg->getFailureCode();

			QUuid re = fMsg->getReUUID();

			log->logINFO("GSClient", "Recv'ed A FailureMsg with code: " +QString::number( fc) + " (" + QString::number(fc, 16)+ ")");
			return true;
		}
	case PING:
		{
			Portal* p = msg->getOrigin();

			if (p != NULL) {
				QString remNodeName = p->getRemoteNodeName();
				log->logINFO("GSClient", "PING from: '" + remNodeName + "'");
				PongMsg pongMsg((PingMsg*)msg);
				p->send(&pongMsg);
			} else {
				log->logINFO("GSClient", "Can't return ping.  NULL Portal*");
			}
			return true;
		}
	case PONG:
		{
			Portal* p = msg->getOrigin();
			PongMsg* pongMsg = (PongMsg*)msg;

			/* calc current and differential times */
			quint64 start = pongMsg->getStartTime();
			quint64 now = Logger::getCurrentTime();
			quint64 diff = now -start;

			QString time = "roundtrip time: " + QString::number(diff) + "ms.";
			QString remNodeName = "unknown";

			if (p != NULL) {
				remNodeName = p->getRemoteNodeName();
			}

			log->logINFO("GSClient", "Pong from: '" + remNodeName + "', " + time);
			return true;
		}
	}
	return false;
}

PortalManager*
GSClient::getPortMan()
{
	return this->portMan;
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
