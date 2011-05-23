/*             C L I E N T C M D R E G I S T R Y . H
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
/** @file ClientCmdRegistry.h
 *
 * Brief description
 *
 */

#ifndef __CLIENTCMDREGISTRY_H__
#define __CLIENTCMDREGISTRY_H__

#include "Logger.h"

#include <string>
#include <list>
#include <map>
#include <GSThread.h>

class AbstractClientCmd;

class ClientCmdRegistry
{
public:
	static ClientCmdRegistry* getInstance();
	virtual ~ClientCmdRegistry();

	bool registerCmd(AbstractClientCmd* cmd);
	AbstractClientCmd* getCmd(std::string cmd);

	std::list<std::string>* getListOfCmds();

private:
	static ClientCmdRegistry* pInstance;
	ClientCmdRegistry();
	Logger* log;

	GSMutex mapLock;
	std::map<std::string,AbstractClientCmd*>* cmdMap;

	/* Disable copy cstr and =operator */
	ClientCmdRegistry(ClientCmdRegistry const&){};
	ClientCmdRegistry& operator=(ClientCmdRegistry const&){};
};

#endif /* __CLIENTCMDREGISTRY_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
