/*                 B A S I C J M T E S T . C X X
 * BRLCAD
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
/** @file configTest.cxx
 *
 * Brief description
 *
 */

#include "Logger.h"
#include "Config.h"

#include <string>

int main(int argc, char* argv[])
{
	Logger* log = Logger::getInstance();
	log->logINFO("ConfigTest", "Attempting to open a file...");

	Config* c = Config::getInstance();

	c->loadFile("test.config");

	QList<std::string> keys = c->getAllKeys();

	 for (int i = 0; i < keys.size(); ++i) {
	     std::string key = keys.at(i);
	     std::string value = c->getConfigValue(key);

	     log->logINFO("ConfigTest", "Read " + key + " value: " + value);
	 }

	return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
