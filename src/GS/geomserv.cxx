/*                     G E O S E R V . C X X
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
/** @file geomserv.cxx
 *
 */

#include "GeometryService.h"
#include "FileDataSource.h"

#include "JobManager.h"
#include "Config.h"

#include <iostream>
#include <stdlib.h>
#include <algorithm>


int gsExit(int code)
{
    Logger* log = Logger::getInstance();
    log->logBANNER("geomserv", "GeometryService is Shutting Down...");

    JobManager::getInstance()->shutdown(true);

    log->logINFO("geomserv", "Exiting.");
    usleep(1000); /* Yeild main thread, let other threads finish unlocking */
    exit(code);
}


int main(int argc, char* argv[])
{
    std::cout << std::endl << std::endl;

    DataManager* dm = DataManager::getInstance();
    Logger::getInstance();
    JobManager::getInstance()->startup();

    Logger* log = Logger::getInstance();
    log->logBANNER("geomserv", "GeometryService Config Loader");

    Config* c = Config::getInstance();

    /* Load configs from File */
    bool goodLoad = c->loadFile("geomserv.config", true);

    if (! goodLoad) {
	log->logERROR("geomserv", "Failed to properly Load config File.  Exiting.");
	gsExit(1);
    }

    /* Check for a local node name.  This is imperative to be set. */
    std::string localNodeName(c->getConfigValue("LocalNodeName"));
    if (localNodeName.length() == 0) {
	log->logERROR("geomserv", "Config File does not contain a 'LocalNodeName' parameter");
	gsExit(1);
    }

    log->logBANNER("geomserv", "Booting GeometryService: " + localNodeName);

    /* Get Listen Port */
    uint16_t listenPort = c->getConfigValueAsUShort("ListenPort");

    if (listenPort <= 0) {
	log->logERROR("geomserv", "Config File does not contain a 'ListenPort' parameter, using default");
	listenPort = DEFAULT_LISTEN_PORT;
    }

    /* Get Listen Addy */
    std::string listenAddy = c->getConfigValue("ListenAddress");

    if (listenAddy.length() <= 0) {
	log->logERROR("geomserv", "Config File does not contain a 'ListenAddress' parameter, using default");
	listenAddy = DEFAULT_LISTEN_ADDY;
    }


    if (dm->init(c) == false) {
	gsExit(1);
    }


    GeometryService gs (localNodeName, listenAddy, listenPort);
    gs.run(); /* blocks */

    log->logINFO("geomserv", "Exiting...");
    return 0;
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
