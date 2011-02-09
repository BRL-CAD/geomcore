/*                  N E T M S G R O U T E R . H
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
/** @file NetMsgRouter.h
 *
 * Brief description
 *
 */

#ifndef __NETMSGROUTER_H__
#define __NETMSGROUTER_H__

#include "NetMsgTypes.h"
#include "INetMsgHandler.h"
#include "NetMsg.h"

#include <QtCore/QMap>
#include <QtCore/QList>
#include <QtCore/QMutex>

class NetMsgRouter
{
public:
	static NetMsgRouter* getInstance();
	virtual ~NetMsgRouter();

	bool registerType(uint16_t type, INetMsgHandler* handler);
	bool routeMsg(NetMsg* msg);

private:
	static NetMsgRouter* pInstance;
	NetMsgRouter();

	static void registerInternalTypes();
	/*
	 * Gets a list of INetMsgHandler pointers associated with
	 * this msgType.  If no Mapping or list exists, one is made.
	 */
	QList<INetMsgHandler*>* getListOfHandlers(uint16_t type);

	QMutex mapLock;
	QMap<uint16_t,QList<INetMsgHandler*>*>* routingTable;

	/* Disable copy cstr and =operator */
	NetMsgRouter(NetMsgRouter const&){};
	NetMsgRouter& operator=(NetMsgRouter const&){};
};

#endif /* __NETMSGROUTER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
