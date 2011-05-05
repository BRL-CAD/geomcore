/*                  L O G G E R T E S T . C X X
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
/** @file loggerTest.cxx
 *
 * Brief description
 *
 */

#include "Logger.h"

int main(int argc, char* argv[])
{
    Logger* log = Logger::getInstance();
    log->logDEBUG("LoggerTest", "Testing DEBUG");
    log->logINFO("LoggerTest", "Testing INFO");
    log->logWARNING("LoggerTest", "Testing WARNING");
    log->logERROR("LoggerTest", "Testing ERROR");
    log->logFATAL("LoggerTest", "Testing FATAL");
    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
