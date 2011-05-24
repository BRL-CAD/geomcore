/*                       L I S T C M D . H
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
/** @file ListCmd.h
 *
 * Brief description
 *
 */

#ifndef __LISTCMD_H__
#define __LISTCMD_H__

#include "AbstractClientCmd.h"
#include <string>

class ListCmd : public AbstractClientCmd {
public:
	ListCmd();
	virtual ~ListCmd();

	std::string getUsage();
	std::string getHelp();

protected:
	bool _exec(GSCmdLineClient* client, std::list<std::string> args);
};

#endif /* __LISTCMD_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
