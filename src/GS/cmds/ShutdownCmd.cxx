/*                 S H U T D O W N C M D . C X X
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
/** @file ShutdownCmd.cxx
 * ShutdownCmd.cxx
 *
 */

#include "ShutdownCmd.h"
#include "NetMsgTypes.h"

ShutdownCmd::ShutdownCmd() : AbstractClientCmd("shutdown"){}
ShutdownCmd::~ShutdownCmd() {}

QString
ShutdownCmd::getUsage(){
	return "Usage: shutdown";
}

QString
ShutdownCmd::getHelp(){
	return "Sends a remote shutdown command to the GS server currently attached.";
}

bool
ShutdownCmd::_exec(GSClient* client, QStringList args){
	Portal* p = client->getCurrentPortal();

	/* Check to see if we are connected */
	if (p == NULL) {
		this->log->logWARNING("ShutdownCmd","Current Portal returned NULL.  Cannot send Shutdown Msg.");
		return false;
	}

	/* Send Shutdown Msg */
	TypeOnlyMsg tom(CMD_SHUTDOWN);
	p->send(&tom);

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
