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
/** @file Config.h
 *
 * Brief description
 *
 */

#ifndef __CONFIG_H__
#define __CONFIG_H__

#include "commonDefines.h"

#include "Logger.h"

#include <map>
#include <string>
#include <list>

#include <bu.h>

class Config
{
public:
	~Config();
	static Config* getInstance();

	bool loadFile(std::string pathAndFileName, bool verbose = false);
	std::string getConfigValue(std::string key);
	void updateValue(std::string key, std::string value);
	std::list<std::string>* getAllKeys();

private:
	Config(); /* Turn off Default cstr */
	Config(const Config& c){}; /* Turn off Copy cstr */
	Config& operator=(const Config& c){}; /* Turn off equal oper */

	std::string processLine(std::string line);
	void removeAllOccurances(std::string* data, std::string search,
			std::string replace);

	Logger* log;
	std::map<std::string, std::string>* configMap;

	static Config* pInstance;
};

#endif /* __CONFIG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
