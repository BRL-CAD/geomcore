/*           C L I E N T C M D R E G I S T R Y . C X X
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
/** @file ClientCmdRegistry.cxx
 *
 * Brief description
 *
 */


#include "ClientCmdRegistry.h"
#include "AbstractClientCmd.h"

#include <GSThread.h>

ClientCmdRegistry* ClientCmdRegistry::pInstance = NULL;

ClientCmdRegistry*
ClientCmdRegistry::getInstance() {
	if (ClientCmdRegistry::pInstance == NULL) {
		ClientCmdRegistry::pInstance = new ClientCmdRegistry();
	}
	return ClientCmdRegistry::pInstance;
}

ClientCmdRegistry::ClientCmdRegistry() {
	this->cmdMap = new std::map<std::string,AbstractClientCmd*> ();
	this->log = Logger::getInstance();
}

ClientCmdRegistry::~ClientCmdRegistry() {
	/* TODO loop thru and del all the CMDs?? */
	delete cmdMap;
}

bool ClientCmdRegistry::registerCmd(AbstractClientCmd* cmd) {
	std::string cmdString = cmd->getCmd();

	GSMutexLocker(&this->mapLock);
	if(cmdMap->insert(std::pair<std::string,AbstractClientCmd*>(cmdString, cmd)).second != false)
		return true;
	this->log->logWARNING("ClientCmdRegistry","An AbstractClientCmd '" + cmdString + "' already exists in Registry.");
	return false;
}

AbstractClientCmd*
ClientCmdRegistry::getCmd(std::string cmd)
{
	GSMutexLocker(&this->mapLock);

	if (this->cmdMap->find(cmd) == cmdMap->end())
		return NULL;

	return this->cmdMap->find(cmd)->second;
}

std::list<std::string>*
ClientCmdRegistry::getListOfCmds() {
	GSMutexLocker(&this->mapLock);

	std::list<std::string>* keys = new std::list<std::string>();
	for(std::map<std::string,AbstractClientCmd*>::iterator it=cmdMap->begin(); it!=cmdMap->end(); it++)
		keys->push_back(it->first);

	return keys;
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

