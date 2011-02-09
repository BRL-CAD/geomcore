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
#include <QtCore/QTime>

#include "brlcad/bu.h"

#include <sys/time.h>
#include <iomanip>
#include <sstream>

//Statics instantiation
Logger* Logger::instance;
QMutex* Logger::lock = new QMutex();

Logger::Logger() :
	verbose(false), printToFile(false), printToConsole(true) {}

Logger* Logger::getInstance() {
	QMutexLocker locker(Logger::lock);

	if (Logger::instance == NULL) {
		Logger::instance = new Logger();
	}

	return Logger::instance;
}

void Logger::logBANNER(std::string origin, std::string string) {
	this->log(Logger::BANNER, origin, string);
}

void Logger::logDEBUG(std::string origin, std::string string) {
	this->log(Logger::DEBUG, origin, string);
}

void Logger::logINFO(std::string origin, std::string string) {
	this->log(Logger::INFO, origin, string);
}

void Logger::logWARNING(std::string origin, std::string string) {
	this->log(Logger::WARNING, origin, string);
}

void Logger::logERROR(std::string origin, std::string string) {
	this->log(Logger::ERROR, origin, string);
}

void Logger::logFATAL(std::string origin, std::string string) {
	this->log(Logger::FATAL, origin, string);
}

void Logger::log(quint32 logLevel, std::string origin, std::string string) {
	std::string time("");

	std::string type("");

	time += QTime::currentTime().toString().toStdString();

	switch (logLevel) {
	case (Logger::FATAL):
		type += "(FATAL) ";
		break;
	case (Logger::ERROR):
		type += "(ERROR) ";
		break;
	case (Logger::WARNING):
		type += "(WARNING) ";
		break;
	case (Logger::INFO):
		type += "(INFO) ";
		break;
	case (Logger::BANNER):
		type += "(BANNER) ";
		break;
	case (Logger::DEBUG):
	default:
		type += "(DEBUG) ";
		break;
	}

	QMutexLocker locker(Logger::lock);

	if (this->printToFile) {
		//TODO add file logging
	}

	std::ostringstream out("");

	if (this->printToConsole) {
		out << std::setw(12) << std::setfill(' ') << std::left << time;
		out << std::setw(20) << std::setfill(' ') << std::left
				<< origin;
		out << std::setw(12) << std::setfill(' ') << std::left << type;

		if (logLevel == Logger::BANNER) {
			out << std::setfill(' ') << std::left << "======= " << string << " =======";
		} else {
			out << std::setfill(' ') << std::left << string;
		}

		if (this->verbose) {
			out << std::setfill(' ') << " \t" << "STACK TRACE GOES HERE";
		}

		out << std::endl;

		std::string std_string = out.str();
		const char* c_string = std_string.c_str();

		bu_log("%s", c_string);
	}
}

quint64
Logger::getCurrentTime()
{
	timeval tim;
	gettimeofday(&tim, NULL);
	quint64 now = (tim.tv_sec * 1000 ) + (tim.tv_usec/1000);
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
