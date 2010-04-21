/*                         L O G G E R . C X X
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
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

#include "utility/Logger.h"
#include <QTime>
#include <iomanip>

//Statics instantiation
Logger* Logger::instance;
QMutex* Logger::lock = new QMutex();

Logger::Logger()
{
    this->verbose = false; //Default to false
}
Logger* Logger::getInstance()
{
    QMutexLocker locker(Logger::lock);

    if (Logger::instance == NULL) {
	Logger::instance = new Logger();
    }

    return Logger::instance;
}

void Logger::logDEBUG(QString origin, QString string)
{
    this->log(Logger::DEBUG, origin, string);
}

void Logger::logINFO(QString origin, QString string)
{
    this->log(Logger::INFO, origin, string);
}

void Logger::logWARNING(QString origin, QString string)
{
    this->log(Logger::WARNING, origin, string);
}

void Logger::logERROR(QString origin, QString string)
{
    this->log(Logger::ERROR, origin, string);
}

void Logger::logFATAL(QString origin, QString string)
{
    this->log(Logger::FATAL, origin, string);
}

void Logger::log(quint32 logLevel, QString origin, QString string)
{
    QString time("");

    QString type("");

    time += QTime::currentTime().toString();
    time += ": ";

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
    case (Logger::DEBUG):
    default:
	type += "(DEBUG) ";
	break;
    }

    QMutexLocker locker(Logger::lock);

    //TODO add file logging

    std::cout << std::setw(12) << std::left << time.toStdString();
    std::cout << std::setw(20) << std::left << origin.toStdString();
    std::cout << std::setw(12) << std::left << type.toStdString();
    std::cout << std::left << string.toStdString();

    if (this->verbose) {
	std::cout << " \t" << "STACK TRACE GOES HERE";
    }

    std::cout << std::endl;

}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8