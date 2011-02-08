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
 * Brief description
 *
 */

#include "GSCmdLineClient.h"

#include "AbstractClientCmd.h"
#include "ExitCmd.h"
#include "HelpCmd.h"
#include "LoginCmd.h"
#include "LogoutCmd.h"
#include "ShutdownCmd.h"
#include "PingCmd.h"
#include "Portal.h"

const std::string GSCmdLineClient::defaultPrompt ="geoclient> ";

GSCmdLineClient::GSCmdLineClient(QString localNodeName):GSClient(localNodeName)
{
	this->ccReg = ClientCmdRegistry::getInstance();
	this->registerClientCmds();
	this->stayRun = true;
	this->prompt = defaultPrompt;
	this->currentPortal = NULL;
}

GSCmdLineClient::~GSCmdLineClient() {
}


int
GSCmdLineClient::run()
{
	this->log->logBANNER("geoclient","==================================");
	this->log->logBANNER("geoclient","GeometryService Test/Stress Client");
	this->log->logBANNER("geoclient","==================================");

	std::string in;
	while (this->stayRun) {
		in == "";
		std::cout << prompt;
		getline (std::cin, in);

		/* make a qstring */
		QString qin(in.c_str());

		/* cathc zero length strings here */
		if (qin.length() == 0)
			continue;

	    /* split string */
		QStringList list = qin.split(" ");

		/* check to see if there is at least one element */
		if (list.length() <= 0) {
			continue;
		}

		/* convert to lowercase */
		QString cmd = list.takeFirst().toLower();

		this->execCmd(cmd, list);
	}

	if (this->currentPortal != NULL)
		this->currentPortal->disconnect();

	if (this->portMan != NULL)
		this->portMan->shutdown();

	this->log->logINFO("geoclient","Exiting.");
	return 0;
}


void
GSCmdLineClient::stopRun()
{
	this->stayRun = false;
}


bool
GSCmdLineClient::execCmd(QString cmd, QStringList args)
{
	AbstractClientCmd* acc = this->ccReg->getCmd(cmd);

	if (acc == NULL) {
		this->log->logINFO("GSClient", "Unknown Command: '" + cmd + "'.");
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
