/*                         L O G G E R . C X X
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
/** @file Logger.cxx
 *
 * Brief description
 *
 */

#include "Logger.h"

#include "bu.h"

#include <sys/time.h>
#include <iomanip>
#include <sstream>

//Statics instantiation
Logger* Logger::instance;
Logger::Logger() :
	printToFile(false), printToConsole(true) {}

Logger* Logger::getInstance() {
	if (Logger::instance == NULL) {
		Logger::instance = new Logger();
	}

	return Logger::instance;
}

void Logger::logBANNER(std::string origin, std::string string) {
	this->log("(BANNER)", origin, string);
}

void Logger::logDEBUG(std::string origin, std::string string) {
	this->log("(DEBUG)", origin, string);
}

void Logger::logINFO(std::string origin, std::string string) {
	this->log("(INFO)", origin, string);
}

void Logger::logWARNING(std::string origin, std::string string) {
	this->log("(WARNING)", origin, string);
}

void Logger::logERROR(std::string origin, std::string string) {
	this->log("(ERROR)", origin, string);
}

void Logger::logFATAL(std::string origin, std::string string) {
	this->log("(FATAL)", origin, string);
}

void Logger::log(std::string lvl, std::string origin, std::string string) {

    if (this->printToFile) {
	//TODO add file logging
    }

    if (this->printToConsole) {
	time_t t ;
	time(&t);
	std::string out(ctime(&t));
	out.erase(out.end()-1,out.end());

	out += '\t';
	out += lvl + '\t';
	out += (lvl[1]=='B'?"======":"");
	out += string;
	out += (lvl[1]=='B'?"======":"");
	out += "\n";
	std::cout << out;
    }
}

uint64_t
Logger::getCurrentTime()
{
	timeval tim;
	gettimeofday(&tim, NULL);
	uint64_t now = (tim.tv_sec * 1000 ) + (tim.tv_usec/1000);
	return now;
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
