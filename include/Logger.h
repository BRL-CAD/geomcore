/*                         L O G G E R . H
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
/** @file Logger.h
 *
 * Brief description
 *
 */

#ifndef __LOGGER_H__
#define __LOGGER_H__

#include <iostream>
#include <stdint.h>
#include <QtCore/QThread>
#include <string>
#include <QtCore/QMutex>
#include <QtCore/QMutexLocker>

class Logger
{
public:
	static Logger* getInstance();
	virtual ~Logger(){};

	void enableVerbose()
	{
		this->verbose = true;
	}
	void disableVerbose()
	{
		this->verbose = false;
	}

	void enableLogToConsole()
	{
		this->printToConsole = true;
	}
	void disableLogToConsole()
	{
		this->printToConsole = false;
	}

	void enableLogToFile()
	{
		this->printToFile = true;
	}
	void disableLogToFile()
	{
		this->printToFile = false;
	}

	void logBANNER(std::string origin, std::string string);
	void logDEBUG(std::string origin, std::string string);
	void logINFO(std::string origin, std::string string);
	void logWARNING(std::string origin, std::string string);
	void logERROR(std::string origin, std::string string);
	void logFATAL(std::string origin, std::string string);

	enum
	{
		BANNER, INFO, WARNING, ERROR, FATAL, DEBUG
	};

	static uint64_t getCurrentTime();

private:
	static Logger* instance;
	static QMutex* lock;
	bool verbose;
	bool printToConsole;
	bool printToFile;

	Logger(); /* Disable Default cstr */
	Logger(const Logger& logger){}; /* Disable Copy cstr */
	Logger& operator=(const Logger& log){}; /* Disable equals operator */

	void log(uint32_t logLevel, std::string origin, std::string string);
};

#endif /* __LOGGER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
