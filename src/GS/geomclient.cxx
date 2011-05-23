/*                   G E O C L I E N T . C X X
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
/** @file geomclient.cxx
 *
 */

#include "GSClient.h"
#include "GSCmdLineClient.h"
#include "Config.h"

int
gsExit(int code)
{
    Logger* log = Logger::getInstance();
    log->logBANNER("geomclient", "GSClient is Shutting Down...");

    JobManager::getInstance()->shutdown(true);

    log->logINFO("geomclient", "Exiting.");
    usleep(1000); /* Yeild main thread, let other threads finish unlocking */
    exit(code);
}

int
main(int argc, char* argv[])
{
    std::cout << std::endl << std::endl;

    Logger::getInstance();
    JobManager::getInstance()->startup();

    Logger* log = Logger::getInstance();
    log->logBANNER("geomclient", "GSClient Config Loader");

    Config* c = Config::getInstance();

    /* Load configs from File */
    bool goodLoad = c->loadFile("geomclient.config", true);

    if (! goodLoad) {
	log->logERROR("geomclient", "Failed to properly Load config File.  Exiting.");
	gsExit(1);
    }

    /* Check for a local node name.  This is imperative to be set. */
    std::string localNodeName = c->getConfigValue("LocalNodeName");
    if (localNodeName.length() == 0) {
	log->logERROR("geomclient", "Config File does not contain a 'LocalNodeName' parameter");
	gsExit(1);
    }

    GSCmdLineClient gsClient(localNodeName);
    int retVal =  gsClient.run();

    JobManager::getInstance()->shutdown(true);

    return retVal;
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
