/*             G S C M D L I N E C L I E N T . C X X
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
/** @file GSCmdLineClient.cxx
 *
 */

#include "GSCmdLineClient.h"

#include <string>
#include <list>
#include <sstream>
#include <algorithm>

#include "AbstractClientCmd.h"
#include "Portal.h"
#include "ListCmd.h"
#include "ExitCmd.h"
#include "HelpCmd.h"
#include "LoginCmd.h"
#include "LogoutCmd.h"
#include "ShutdownCmd.h"
#include "PingCmd.h"
#include "GetCmd.h"

const std::string GSCmdLineClient::defaultPrompt ="geoclient> ";

GSCmdLineClient::GSCmdLineClient(std::string localNodeName):GSClient(localNodeName)
{
    this->ccReg = ClientCmdRegistry::getInstance();
    this->registerClientCmds();
    this->stayRun = true;
    this->prompt = defaultPrompt;
    this->currentPortal = NULL;
}

GSCmdLineClient::~GSCmdLineClient()
{
}

int
GSCmdLineClient::run()
{
    this->log->logBANNER("geoclient", "==================================");
    this->log->logBANNER("geoclient", "GeometryService Test/Stress Client");
    this->log->logBANNER("geoclient", "==================================");

    std::string in;
    while (this->stayRun) {
	in == "";
	std::cout << prompt;
	getline (std::cin, in);

	/* Catch zero length strings here */
	if (in.length() == 0)
	    continue;

	/* split string */
	std::istringstream iss(in.c_str());
	std::list<std::string> list;
	do { std::string tok; iss>>tok; list.push_back(tok);} while (iss);
	list.pop_back(); /* remove the empty end */

	/* check to see if there is at least one element */
	if (list.size() <= 0) {
	    continue;
	}

	/* convert to lowercase */
	std::string cmd(*(list.begin()));
	std::transform(cmd.begin(), cmd.end(), cmd.begin(), ::tolower);

	list.pop_front(); /* remove the cmd from the front */

	this->execCmd(cmd, list);
    }

    if (this->currentPortal != NULL)
	this->currentPortal->disconnect();

    if (this->portMan != NULL)
	this->portMan->shutdown();

    this->log->logINFO("geoclient", "Exiting.");
    return 0;
}

void
GSCmdLineClient::stopRun()
{
    this->stayRun = false;
}

bool
GSCmdLineClient::execCmd(std::string cmd, std::list<std::string> args)
{
    char buf[BUFSIZ];

    AbstractClientCmd* acc = this->ccReg->getCmd(cmd);

    if (acc == NULL) {
	snprintf(buf, BUFSIZ, "Unknown Command: '%s'", cmd.c_str());
	this->log->logINFO("GSClient", buf);
	return false;
    }

    return acc->exec(this, args);
}

void
GSCmdLineClient::registerClientCmds()
{
    /* Command Registrations */
    this->ccReg->registerCmd(new HelpCmd());
    this->ccReg->registerCmd(new ExitCmd());
    this->ccReg->registerCmd(new LoginCmd());
    this->ccReg->registerCmd(new LogoutCmd());
    this->ccReg->registerCmd(new ShutdownCmd());
    this->ccReg->registerCmd(new PingCmd());
    this->ccReg->registerCmd(new GetCmd());
    this->ccReg->registerCmd(new ListCmd());
}

bool
GSCmdLineClient::setCurrentPortal(Portal* p)
{
    if (p != NULL) {
	this->currentPortal = p;
	return true;
    }
    return false;
}

Portal*
GSCmdLineClient::getCurrentPortal()
{
    return this->currentPortal;
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
