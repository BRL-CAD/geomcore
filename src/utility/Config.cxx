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
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QStringList>
#include <QtCore/QMutexLocker>

Config* Config::pInstance = NULL;

Config::Config()
{
    this->configMap = new QMap<std::string, std::string> ();
    this->log = Logger::getInstance();
}

Config::~Config()
{
    delete this->configMap;
}

Config* Config::getInstance()
{
    if (!Config::pInstance) {
    	pInstance = new Config();
    }
    return Config::pInstance;
}

bool Config::loadFile(std::string pathAndFileName, bool verbose)
{
    QString pth;
    std::string msg;
    msg = "Attemping to load config from: '" + pathAndFileName + "'.";
    this->log->logINFO("Config", msg);

    //init file object
    pth.append(pathAndFileName.c_str());
    QFile f(pth);

    if (f.exists() == false) {
		msg = "Could not find file: '" + pathAndFileName + "'.";
		this->log->logFATAL("Config", msg);
		return false;
    }


    //verify & open
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) {
		msg = "Loading config from: '" + pathAndFileName + "' FAILED.";
		this->log->logFATAL("Config", msg);
		return false;
    }

    while (!f.atEnd()) {
		QByteArray lineBytes = f.readLine();

		std::string line(lineBytes);

		//Rem newline:
		this->removeAllOccurances(&line, "\n", "");

		//Check for comments
		if (line[0] == '#') {
			//log->logINFO("Config", "Ignoring Comment. (" + line + ")");
		} else {
			std::string key = this->processLine(line);

			if (verbose && key.length() > 0) {
				std::string value = this->configMap->value(key);
				log->logINFO("Config", "Read key/value: '" + key + "'->'" + value + "'");
			}

		}
    }
    QFileInfo info(f);

    log->logINFO("Config", "Done loading config from: " + info.absoluteFilePath().toStdString());
    return true;
}

std::string
Config::processLine(std::string line)
{
    char key[BUFSIZ], value[BUFSIZ];

    switch(sscanf(line.c_str(), "%s %s", &key, &value)) {
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

void Config::removeAllOccurances(std::string* data, std::string search, std::string replace)
{
	/*
    while (data->contains(search)) {
	*data = (*data).replace(search, replace);
    }
    */
}

std::string Config::getConfigValue(std::string key)
{
	QMutexLocker(&this->mapLock);
    return this->configMap->value(key, "") + "";
}

void
Config::updateValue(std::string key, std::string value)
{
	QMutexLocker(&this->mapLock);
	this->configMap->insert(key, value);
}

QList<std::string> Config::getAllKeys()
{
	QMutexLocker(&this->mapLock);
	return this->configMap->uniqueKeys();
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
