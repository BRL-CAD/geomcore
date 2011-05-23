/*                   L O G O U T C M D . C X X
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
/** @file LogoutCmd.cxx
 *
 */

#include "LogoutCmd.h"
#include "Portal.h"
#include "PortalManager.h"
#include "TypeOnlyMsg.h"
#include "NetMsgTypes.h"

LogoutCmd::LogoutCmd() : AbstractClientCmd("logout") {}

LogoutCmd::~LogoutCmd() {}

std::string
LogoutCmd::getUsage()
{
    return "Usage: logout";
}

std::string
LogoutCmd::getHelp()
{
    return "Severs the connection to the current GeometryService.";
}

bool
LogoutCmd::_exec(GSCmdLineClient* client, std::list<std::string> args)
{
    Portal* p = client->getCurrentPortal();

    /* Check to see if we are connected */
    if (p == NULL) {
	this->log->logWARNING("Logout", "Not connected to a Geometry Service.");
	return false;
    }

    TypeOnlyMsg msg(DISCONNECTREQ);
    p->send(&msg);

    /* This is an ugly way to do this, 'friend' is needed in GSClient.h */
    client->currentPortal = NULL;

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
