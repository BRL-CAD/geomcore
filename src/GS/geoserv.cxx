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
/** @file geoserv.cxx
 *
 * Brief description
 *
 */

#include "GeometryService.h"
#include "FileDataSource.h"

#include "JobManager.h"
#include "Config.h"

#include <iostream>
#include <stdlib.h>


int gsExit(int code)
{
	Logger* log = Logger::getInstance();
    log->logBANNER("geoserv", "GeometryService is Shutting Down...");

	JobManager::getInstance()->shutdown(true);

	log->logINFO("geoserv", "Exiting.");
	usleep(1000); /* Yeild main thread, let other threads finish unlocking */
	exit(code);
}

int main(int argc, char* argv[])
{
    std::cout << std::endl << std::endl;

    Logger::getInstance();
	JobManager::getInstance()->startup();

    Logger* log = Logger::getInstance();
    log->logBANNER("geoserv", "GeometryService Config Loader");

    Config* c = Config::getInstance();

    /* Load configs from File */
    bool goodLoad = c->loadFile("geoserv.config", true);

    if ( ! goodLoad) {
    	log->logERROR("geoserv","Failed to properly Load config File.  Exiting.");
    	gsExit(1);
    }

    /* Check for a local node name.  This is imperative to be set. */
    std::string localNodeName(c->getConfigValue("LocalNodeName"));
    if (localNodeName.length() == 0) {
		log->logERROR("geoserv", "Config File does not contain a 'LocalNodeName' parameter");
		gsExit(1);
	}

    log->logBANNER("geoserv", "Booting GeometryService: " + localNodeName);

    std::string sPort = c->getConfigValue("ListenPort");
    if (sPort.length() == 0){
    	log->logERROR("geoserv", "Config File does not contain a 'ListenPort' parameter");
    	gsExit(1);
    }
    if (sPort.length() <= 0){
    	log->logERROR("geoserv", "Config File contains a 'ListenPort' key, however the value length was <= 0.");
    	gsExit(1);
    }

    bool ok;
    uint16_t port = atoi(sPort.c_str());
    if (!ok){
    	log->logERROR("geoserv", "Config File contains a 'ListenPort' key, however the value failed to parse to a valid number.");
    	return 1;
    }


    GeometryService gs (localNodeName, port);

    /* DataManager elements. */
    std::string useFileRepo(c->getConfigValue("UseFileRepo"));
     if (useFileRepo == "yes" || useFileRepo == "true"){
    	std::string fileRepoPath(c->getConfigValue("FileRepoPath"));

    	if (fileRepoPath.length() == 0) {
         	log->logERROR("geoserv", "FileRepo was flagged for use, but no 'FilePathRepo' var was configured.");
    		return 1;
    	}

     	log->logINFO("geoserv", "FileDataSouce being used.");
        FileDataSource* fds = new FileDataSource(fileRepoPath);
        gs.getDataManager()->addDataSource(fds);
     }


    gs.run(); /* blocks */

	log->logINFO("geoserv", "Exiting...");
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
