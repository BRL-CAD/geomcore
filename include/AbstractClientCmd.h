/*             A B S T R A C T C L I E N T C M D . H
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
/** @file AbstractClientCmd.h
 * AbstractClientCmd.h
 *
 */

#ifndef __ABSTRACTCLIENTCMD_H__
#define __ABSTRACTCLIENTCMD_H__

#include "Logger.h"
#include "GSCmdLineClient.h"

#include <string>

class AbstractClientCmd {
public:
	virtual ~AbstractClientCmd();
	bool exec(GSCmdLineClient* client, std::list<std::string> args);

	std::string getCmd();
	virtual std::string getUsage() = 0;
	virtual std::string getHelp() = 0;

	void printUsage();
	void printHelp();

protected:
	AbstractClientCmd(std::string cmd);
	AbstractClientCmd(AbstractClientCmd* acCmd);

	virtual bool _exec(GSCmdLineClient* client, std::list<std::string> args) = 0;

	Logger* log;

private:
	std::string cmd;
};

#endif /* __ABSTRACTCLIENTCMD_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
