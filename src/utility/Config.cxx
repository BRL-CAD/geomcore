/*                   C O N F I G . H
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
/** @file Config.cxx
 *
 * Configuration class that provides loading and storing of configuration Key/Value pairs.
 *
 */

#include "Config.h"

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

Config* Config::pInstance = NULL;

Config::Config()
{
    this->configMap = new std::map<std::string, std::string> ();
    this->log = Logger::getInstance();
}

Config::~Config()
{
    delete this->configMap;
}

Config*
Config::getInstance()
{
    if (!Config::pInstance) {
    	pInstance = new Config();
    }
    return Config::pInstance;
}

bool
Config::loadFile(std::string pathAndFileName, bool verbose)
{
    FILE *f;
    char buf[BUFSIZ];
    char *realpathptr;
    char realpathstr[MAXPATHLEN];

    this->log->logINFO("Config", "Attemping to load config from: '" + pathAndFileName + "'.");

    if ((f=fopen(pathAndFileName.c_str(), "r")) == NULL) {
	this->log->logFATAL("Config", "Could not find file: '" + pathAndFileName + "'.");
	return false;
    }

    while (!feof(f)) {
	if(fgets(buf, BUFSIZ, f) == NULL) {
	    if (!feof(f))
		log->logINFO("Config", std::string("Error reading file: ") + strerror(errno));
	    break;
	}
	if(*buf == '#')
	    continue;

	std::string key = this->processLine(std::string(buf));
	if (verbose && key.length() > 0)
	    log->logINFO("Config", "Read key/value: '" + key + "'->'" + configMap->find(key)->second + "'");
    }
    
    realpathptr = realpath(pathAndFileName.c_str(), realpathstr);
    log->logINFO("Config", std::string("Done loading config from: ") + realpathptr);
    return true;
}

std::string
Config::processLine(std::string line)
{
    char key[BUFSIZ], value[BUFSIZ];

    switch(sscanf(line.c_str(), "%s %s", key, value)) {
	case -1:
	    return std::string("");
	case 2:
	    this->updateValue(std::string(key), std::string(value));
	    return std::string(key);
	default:
	    this->log->logERROR("Config", "Not enough elements for Key/Value pair on Config Line: " + line);
	    return "";
    }
}

void
Config::removeAllOccurances(std::string* data, std::string search, std::string replace)
{
	/*
    while (data->contains(search)) {
	*data = (*data).replace(search, replace);
    }
    */
}

std::string
Config::getConfigValue(std::string key)
{
    std::string ret = this->configMap->find(key)->second + "";
    return ret;
}

void
Config::updateValue(std::string key, std::string value)
{
    this->configMap->insert(std::pair<std::string,std::string>(key, value));
}

std::list<std::string>*
Config::getAllKeys()
{
	std::list<std::string>* l = new std::list<std::string>();
	std::map<std::string, std::string>::iterator it;

	for(it=configMap->begin() ; it != configMap->end(); it++) {
		std::string s = it->first;
		l->push_back(s);
	}
	return l;
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
