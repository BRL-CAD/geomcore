/*             D A T A M A N A G E R . C X X
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
/** @file DataManager.cxx
 *
 * Single point of access for all Database
 * object IO from SVN, Cache and Disk
 *
 */

#include "DataManager.h"
#include "FileDataSource.h"
#include "NetMsgTypes.h"
#include "TypeOnlyMsg.h"
#include "GeometryManifestMsg.h"

#include <GSThread.h>


DataManager* DataManager::pInstance = NULL;


DataManager::DataManager()
{
    this->log = Logger::getInstance();
}


DataManager::~DataManager()
{}


std::string
DataManager::getObjectByPath(std::string url)
{}


bool
DataManager::setDataSource(IDataSource* source)
{
    if (this->datasource != NULL)
    return false;

    this->datasource = source;
}


bool
DataManager::handleNetMsg(NetMsg* msg)
{
    uint16_t type = msg->getMsgType();
    switch(type) {
    case GEOMETRYREQ:
        this->handleGeometryReqMsg((GeometryReqMsg*)msg);
        return true;
    case GEOMETRYMANIFEST:
        return true;
    case GEOMETRYCHUNK:
        this->handleGeometryChunkMsg((GeometryChunkMsg*)msg);
        return true;
    }
    return false;
}


void
DataManager::handleGeometryChunkMsg(GeometryChunkMsg* msg)
{
    Portal* origin = msg->getOrigin();

    //validate incoming data
    if (origin == 0) {
    //TODO Figure out how to how to handle NULL Portal
    log->logERROR("DataManager", "handleGeometryChunkMsg(): NULL Portal!");
    return;
    }
}


void
DataManager::handleGeometryReqMsg(GeometryReqMsg* msg)
{
    bool recurse = msg->getRecurse();
    std::string path = msg->getPath();
    Portal* origin = msg->getOrigin();

    //validate incoming data
    if (origin == 0) {
        //TODO Figure out how to how to handle NULL Portal
        log->logERROR("DataManager", "handleGeometryReqMsg(): NULL Portal!");
        return;
    }

    if (path.length() == 0) {
    	origin->sendTypeOnlyMessage(BAD_REQUEST, msg);
        return;
    }

    if (this->datasource == NULL) {
    	origin->sendTypeOnlyMessage(OPERATION_NOT_AVAILABLE, msg);
        return;
    }

    /* pull all objects */
    std::list<bu_external*>* objs = this->datasource->getObjs(path);
    if (objs == NULL) {
       	origin->sendTypeOnlyMessage(COULD_NOT_FIND_GEOMETRY, msg);
        return;
    }

    /* Prep for send */
    std::list<GeometryChunkMsg*> msgs;
    GeometryChunkMsg* chunk = NULL;
    int cnt = 0;
    bu_external* obj = NULL;

    for(std::list<bu_external*>::iterator it = objs->begin();
        it != objs->end(); it++)
    {
        obj = *it;
        chunk = DataManager::extToChunk(path, obj);
        //chunk->getByteArray()->printHexString("");
        msgs.push_back(chunk);
    }

    //GeometryManifestMsg man(msgs);


    return;
}


DataManager*
DataManager::getInstance()
{
    if (!DataManager::pInstance)
        DataManager::pInstance = new DataManager();
    return DataManager::pInstance;
}


bool
DataManager::init(Config* c)
{
    std::string repoType = c->getConfigValue("RepoType");
    if (repoType.length() == 0) {
        log->logERROR("DataManager",
              "Config File does not contain a 'RepoType' parameter");
        return false;
    }
    // to lower
    for (int i=0; i < repoType.length(); ++i)
        repoType[i] = std::tolower(repoType[i]);

    /* Attempt to instantiate a DataSource */
    if (repoType == "file") {
        std::string fRepoPath(c->getConfigValue("FileRepoPath"));
        if (fRepoPath.length() == 0)
        {
            log->logERROR("DataManager", "Config File does not contain a 'FileRepoPath' parameter");
            return false;
        }

        log->logINFO("DataManager", "FileDataSouce being used.");
        FileDataSource* fds = new FileDataSource(fRepoPath);

        if (fds->init() == false) {
            log->logERROR("DataManager", "FileDataSouce could not read/write to the path supplied by the 'FileRepoPath' config value.  Please check the existance and permissions of this path.");
            delete fds;
            return false;
        }

        this->setDataSource(fds);
        return true;

    } else if (repoType == "svn") {
        log->logERROR("DataManager", "SVN repoType not implemented yet.");
        return false;

    } else {
        log->logERROR("DataManager", "Invalid RepoType in config file.  Valid values are 'file' and 'svn'");
        return false;
    }
}

GeometryChunkMsg*
DataManager::extToChunk(std::string path, bu_external* ext)
{
	size_t magicLen = sizeof(long);

	size_t size = magicLen + ext->ext_nbytes;
	char* buf = (char*)bu_malloc(size, "objToChunk buf malloc");

	memcpy (buf, &ext->ext_magic, magicLen);
	memcpy (buf + magicLen, ext->ext_buf, ext->ext_nbytes);

	ByteArray ba(buf, size);
	return new GeometryChunkMsg(path, &ba);

//	std::cout << "ext->ext_magic: "<< ext->ext_magic << "\n";
//
//	std::cout << "ext->ext_buf: ";
//	unsigned char* p = (unsigned char*)ext->ext_buf;
//	for (int i = 0; i<ext->ext_nbytes;i++)
//	{
//		printf("%02x ", *p);
//		p++;
//	}
//	std::cout << "\n";
//
//	std::cout << "buf: ";
//	p = buf;
//	for (int i = 0; i<size;i++)
//	{
//		printf("%02x ", *p);
//		p++;
//	}
//	std::cout << "\n";

}

bu_external*
DataManager::chunkToExt(GeometryChunkMsg* msg)
{

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
