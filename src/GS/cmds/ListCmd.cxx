/*                     L I S T C M D . C X X
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
/** @file ListCmd.cxx
 *
 * Brief description
 *
 */

#include "ListCmd.h"
#include "Portal.h"
#include "PortalManager.h"
#include "TypeOnlyMsg.h"
#include "NetMsgTypes.h"
#include "DirListReqMsg.h"

ListCmd::ListCmd() : AbstractClientCmd("list") {}

ListCmd::~ListCmd() {}

std::string
ListCmd::getUsage()
{
    return "Usage: list pathToGeometry";
}

std::string
ListCmd::getHelp()
{
    return "Attempts to get a list of geometry for the provided path from the current GeometryService";
}

bool
ListCmd::_exec(GSCmdLineClient* client, std::list<std::string> args)
{
    int argn = args.size();

    if (argn != 1) {
    	this->printUsage();
    	return false;
    }

    /* Check to see if we are connected */
    Portal* p = client->getCurrentPortal();
    if (p == NULL) {
    	this->log->logWARNING("ListCmd", "Not connected to a Geometry Service.");
		return false;
    }
    std::list<std::string>::iterator it = args.begin();
    std::string path(*it);

    this->log->logINFO("ListCmd", "Attempting to list: '" + path + "'.");

    DirListReqMsg req(path);

    p->send(&req);

    return true;
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
