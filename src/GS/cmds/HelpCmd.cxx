/*                     H E L P C M D . C X X
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
/** @file HelpCmd.cxx
 *
 */

#include "HelpCmd.h"
#include "ClientCmdRegistry.h"


HelpCmd::HelpCmd() : AbstractClientCmd("help") {}


HelpCmd::~HelpCmd() {}


std::string
HelpCmd::getUsage()
{
    return "Usage: help [cmdname]";
}


std::string
HelpCmd::getHelp()
{
    return "If evoked without any arguments, help diplays a list of available commands.  If a command name is provided as an argument, the help for that command is displayed.";
}


bool
HelpCmd::_exec(GSCmdLineClient* client, std::list<std::string> args)
{
    int argn = args.size();

    if (argn < 0 || argn > 1) {
	this->printUsage();
	return false;
    }

    ClientCmdRegistry* ccReg = ClientCmdRegistry::getInstance();

    if (argn == 0) {
	/* display list of cmds */
	/* TODO: fix this. */
	/* something similar to (format nil "~{~a~^, ~}" cmds) */
	std::list<std::string>* cmds = ccReg->getListOfCmds();

	this->log->logINFO("HelpCmd", "Available commands:");

	std::string out("\t");
	std::list<std::string>::iterator it=cmds->begin();
	for (int i = 0; i < cmds->size(); ++i) {
	    /* Append the new cmd name */
	    out.append(*it);
	    it++;

	    /* as long as we are not the last command, append a comma */
	    if (i+1 < cmds->size())
		out.append(", ");

	    /* every 5th command, start a new line. */
	    if ((i+1) % 5 == 0) {
		this->log->logINFO("HelpCmd", out);
		out = "\t"; /* reset for next loop pass */
	    }
	}

	/* flush if anything is left. */
	if (out.length() != 0)
	    this->log->logINFO("HelpCmd", out);


	delete cmds;
	return true;
    } else {
	/* display specifics of a single cmd */
	std::string cmd(*args.begin());

	if (cmd.length() == 0) {
	    this->log->logERROR("HelpCmd", "Zero Length Cmd provided to help.");
	    this->printUsage();
	    return false;
	}

	AbstractClientCmd* acc = ccReg->getCmd(cmd);

	if (acc == NULL) {
	    this->log->logINFO("HelpCmd", "Unknown command: '" + cmd + "'.");
	    this->printUsage();
	    return false;
	}

	acc->printUsage();
	acc->printHelp();

	/* NOTE:  Do NOT delete acc, its used by other objects */

	return true;
    }
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

