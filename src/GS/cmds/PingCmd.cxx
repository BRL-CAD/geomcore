/*                     P I N G C M D . C X X
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
/** @file PingCmd.cxx
 * PingCmd.cxx
 *
 *  Created on: Dec 22, 2010
 */

#include "Portal.h"
#include "PingCmd.h"
#include "PingMsg.h"

PingCmd::PingCmd(): AbstractClientCmd("ping"){}

PingCmd::~PingCmd() {}


QString
PingCmd::getUsage(){
	return "Usage: ping";
}

QString
PingCmd::getHelp(){
	return "Pings the remote host.  Pong is expected in return.";
}

bool
PingCmd::_exec(GSCmdLineClient* client, QStringList args){
	Portal* p = client->getCurrentPortal();

	if (p == NULL) 	{
		this->log->logERROR("PingCmd", "Not connected to a Geometry Service.");
		return false;
	}

	quint64 now = Logger::getCurrentTime();
	PingMsg msg(now);

	p->send(&msg);
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
