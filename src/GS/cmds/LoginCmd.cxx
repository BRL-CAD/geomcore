/*                    L O G I N C M D . C X X
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
/** @file LoginCmd.cxx
 * LoginCmd.cxx
 *
 *  Created on: Dec 16, 2010
 *      
 */

#include "LoginCmd.h"
#include "Portal.h"
#include "PortalManager.h"
#include "NewSessionReqMsg.h"

LoginCmd::LoginCmd() : AbstractClientCmd("login"){}
LoginCmd::~LoginCmd() {}

std::string
LoginCmd::getUsage() {
	return "Usage: login ip port uname passwd";
}

std::string
LoginCmd::getHelp() {
	return "Attempts to make a connection and login to a GeometryService.";
}

bool
LoginCmd::_exec(GSCmdLineClient* client, QStringList args){
	int argn = args.length();

	if (argn != 4) {
		this->printUsage();
		return false;
	}

	/* Convert args to proper types */
 	std::string host(args.at(0).toStdString());
 	uint16_t port = atoi(args.at(1).toStdString().c_str());
 	std::string uname(args.at(2).toStdString());
 	std::string passwd(args.at(3).toStdString());


	if (port <=0 || host.length() == 0 || uname.length() == 0 || passwd.length() == 0){
		this->printUsage();
		return false;
	}

	PortalManager* pm = client->getPortMan();

	if (pm == NULL)	{
		this->log->logERROR("LoginCmd", "NULL PortalManager, cannot connect.");
		return false;
	}

	Portal* p = pm->connectToHost(host, port);

	if (p == NULL) 	{
		this->log->logERROR("LoginCmd", "Failed to open new Portal, cannot connect.");
		return false;
	}

	/* Give the Portal some time to handshake. */
	GSThread::msleep(100);

	client->setCurrentPortal(p);

	/* Authenticate */
	NewSessionReqMsg nsrm(uname, passwd);
	p->send(&nsrm);

	GSThread::msleep(100);

	std::string remNodename = p->getRemoteNodeName();
	this->log->logINFO("LoginCmd", "Connected to: '" + remNodename+ "'.");

	return true;
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

