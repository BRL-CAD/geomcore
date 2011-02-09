/*                 P O R T A L M A N A G E R . H
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
/** @file PortalManager.h
 *
 * Brief description
 *
 */

#ifndef __PORTALMANAGER_H__
#define __PORTALMANAGER_H__

#include "ControlledThread.h"
#include "PkgTcpServer.h"
#include "INetMsgHandler.h"
#include "TypeOnlyMsg.h"
#include "Logger.h"

#include <sys/select.h>

#include <string>

#include <QtCore/QList>
#include <QtCore/QMutex>
#include <string>

class Portal;

class PortalManager : public ControlledThread, public INetMsgHandler
{
public:
	PortalManager(std::string localNodeName, uint16_t port = 0, std::string address = std::string("0.0.0.0"));
	~PortalManager();

	Portal* connectToHost(std::string host, uint16_t port);
	void disconnect(Portal* p);
    bool handleNetMsg(NetMsg* msg);
    std::string getLocalNodeName();

protected:
	void _run();

private:
	std::string localNodeName;
	Logger* log;

	uint16_t listenPort;
	std::string listenAddress;
	PkgTcpServer* tcpServer;

	QMutex masterFDSLock;
	fd_set masterfds;
	int fdmax;

	QMutex* portalsLock;
	QMap<int, Portal*>* fdPortalMap;

	Portal* makeNewPortal(PkgTcpClient* client, struct pkg_switch* table);
	struct pkg_switch* makeNewSwitchTable();
	void closeFD(int fd, std::string logComment);
    void handleDisconnectReqMsg(TypeOnlyMsg* msg);

	/* Disable copy cstr and =operator */
	PortalManager(PortalManager const&){};
	PortalManager& operator=(PortalManager const&){};
};

#endif /* __PORTALMANAGER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
