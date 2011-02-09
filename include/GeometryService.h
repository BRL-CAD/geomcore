/*               G E O M E T R Y S E R V I C E . H
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
/** @file GeometryService.h
 *
 * Brief description
 *
 */

#ifndef __GEOMETRYSERVICE_H__
#define __GEOMETRYSERVICE_H__

#include "DataManager.h"
#include "ControlledThread.h"
#include "PortalManager.h"
#include "NetMsgRouter.h"
#include "FailureMsg.h"

#include <string>

#include <string>

static const uint16_t DEFAULT_LISTEN_PORT = 5309;
static const std::string DEFAULT_LISTEN_ADDY = "0.0.0.0";

class GeometryService : public ControlledThread, public INetMsgHandler
{
public:
	GeometryService(const std::string localNodeName, const uint16_t listenPort = DEFAULT_LISTEN_PORT, const std::string listenAddy = DEFAULT_LISTEN_ADDY);
	virtual ~GeometryService();
    bool handleNetMsg(NetMsg* msg);
    DataManager* getDataManager();

protected:
	bool preRunHook();
	void _run();
	bool postRunHook();

private:
	Logger* log;
	std::string localNodeName;
	uint16_t listenPort;
	std::string listenAddy;

	PortalManager* portalMan;
	DataManager* dataMan;

	void registerMsgRoutes();

	/* Disable copy cstr and =operator */
	GeometryService(GeometryService const&){};
	GeometryService& operator=(GeometryService const&){};
};

#endif /* __GEOMETRYSERVICE_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
