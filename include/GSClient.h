/*                      G S C L I E N T . H
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
/** @file GSClient.h
 * GSClient.h
 *
 */

#ifndef __GSCLIENT_H__
#define __GSCLIENT_H__

#include "NetMsg.h"
#include "PortalManager.h"
#include "JobManager.h"

#include <string>
#include <QtCore/QStringList>

#include <string>
#include <iostream>
#include <stdlib.h>

class GSClient: public INetMsgHandler {
public:
	GSClient(std::string localNodeName);
	virtual ~GSClient();

    bool handleNetMsg(NetMsg* msg);

	PortalManager* getPortMan();

protected:
	void registerMsgRoutes();

	Logger* log;
	JobManager* jobMan;

	PortalManager* portMan;
};

#endif /* __GSCLIENT_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
