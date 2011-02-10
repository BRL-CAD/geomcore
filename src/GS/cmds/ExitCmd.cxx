/*                     E X I T C M D . C X X
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
/** @file ExitCmd.cxx
 * ExitCmd.cxx
 *
 *  Created on: Dec 16, 2010
 */

#include "ExitCmd.h"
#include "Portal.h"

ExitCmd::ExitCmd() : AbstractClientCmd("exit"){}
ExitCmd::~ExitCmd() {}

std::string
ExitCmd::getUsage(){
	return "Usage: exit";
}

std::string
ExitCmd::getHelp(){
	return "Causes the application to exit.";
}

bool
ExitCmd::_exec(GSCmdLineClient* client, std::list<std::string> args){
	/* set the run flag to false */
	client->stopRun();
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

