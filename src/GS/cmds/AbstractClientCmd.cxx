/*           A B S T R A C T C L I E N T C M D . C X X
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
/** @file AbstractClientCmd.cxx
 *
 */

#include "AbstractClientCmd.h"
#include "GSClient.h"


AbstractClientCmd::AbstractClientCmd(std::string cmd) : cmd(cmd)
{
    this->log = Logger::getInstance();
}


AbstractClientCmd::AbstractClientCmd(AbstractClientCmd* acCmd) : cmd(acCmd->getCmd())
{}

AbstractClientCmd::~AbstractClientCmd()
{
}


std::string
AbstractClientCmd::getCmd()
{
    return this->cmd;
}


bool
AbstractClientCmd::exec(GSCmdLineClient* client, std::list<std::string> args)
{
    return this->_exec(client, args);
}


void
AbstractClientCmd::printUsage()
{
    this->log->logINFO(this->cmd, this->getUsage());
}


void
AbstractClientCmd::printHelp()
{
    this->log->logINFO(this->cmd, this->getHelp());
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
