/*               G S C M D L I N E C L I E N T . H
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
/** @file GSCmdLineClient.h
 *
 * Brief description
 *
 */

#ifndef __GSCMDLINECLIENT_H__
#define __GSCMDLINECLIENT_H__

#include "GSClient.h"
#include "ClientCmdRegistry.h"

#include <string>

#include <string>
#include <iostream>
#include <stdlib.h>

class LoginCmd;
class LogoutCmd;
class GSCmdLineClient: public GSClient {
	friend class LoginCmd;
	friend class LogoutCmd;
public:
	GSCmdLineClient(std::string localNodeName);
	virtual ~GSCmdLineClient();

	int run();
	void stopRun();
	bool execCmd(std::string cmd, QStringList args);
	Portal* getCurrentPortal();

protected:
	bool stayRun;
	ClientCmdRegistry* ccReg;
	std::string prompt;
	Portal* currentPortal;
	const static std::string defaultPrompt;

	void registerClientCmds();
	bool setCurrentPortal(Portal* p);

};

#endif /* __GSCMDLINECLIENT_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
