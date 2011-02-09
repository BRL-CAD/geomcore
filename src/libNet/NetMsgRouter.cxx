/*                N E T M S G R O U T E R . C X X
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
/** @file NetMsgRouter.cxx
 *
 * Brief description
 *
 */

#include "NetMsgRouter.h"
#include <QtCore/QMutexLocker>
#include "Portal.h"
#include "FailureMsg.h"

NetMsgRouter* NetMsgRouter::pInstance = NULL;

NetMsgRouter*
NetMsgRouter::getInstance() {
	if (NetMsgRouter::pInstance == NULL) {
		NetMsgRouter::pInstance = new NetMsgRouter();
		NetMsgRouter::pInstance->registerInternalTypes();
	}
	return NetMsgRouter::pInstance;
}

NetMsgRouter::NetMsgRouter() {
	this->routingTable = new QMap<uint16_t, QList<INetMsgHandler*>*> ();
}

NetMsgRouter::~NetMsgRouter() {
	delete routingTable;
}

bool NetMsgRouter::registerType(uint16_t type, INetMsgHandler* handler) {
	/* First get the appropriate list: */
	QList<INetMsgHandler*>* list = this->getListOfHandlers(type);
	list->append(handler);

	return true;
}

bool NetMsgRouter::routeMsg(NetMsg* msg) {
	/* First get the appropriate list: */
	QList<INetMsgHandler*>* list = this->getListOfHandlers(msg->getMsgType());

	char buf[BUFSIZ];
	std::string s;
/*
	QString s("Got a message whos origin is Portal: ");
	Portal* origin = msg->getOrigin();
	if (origin != 0) {
		s.append(origin->getRemoteNodeName());
	} else {
		s.append("NULL origin!");
	}
	s.append(" and type: ");
	s.append(QString::number(msg->getMsgType(),16).toUpper());
	Logger::getInstance()->logINFO("NetMsgRouter", s);
*/

	if (list->length() == 0) {
		/* If no routing table, print an error */
		snprintf(buf, BUFSIZ, "Msg type: %X has no forwarding information.", msg->getMsgType());
		Logger::getInstance()->logWARNING("NetMsgRouter",std::string(buf));
		return false;

	} else {
		for (int i = 0; i < list->length(); ++i) {
			list->at(i)->handleNetMsg(msg);
		}
	}
	/* Now delete msg */
	delete msg;
	return true;
}

QList<INetMsgHandler*>*
NetMsgRouter::getListOfHandlers(uint16_t type) {
	QMutexLocker(&this->mapLock);

	QList<INetMsgHandler*>* l = this->routingTable->value(type);

	if (l == 0) {
		l = new QList<INetMsgHandler*> ();
		this->routingTable->insert(type, l);
	}
	return l;
}

void
NetMsgRouter::registerInternalTypes()
{
	/* TODO add in any type<->Handler associations that should be automatically mapped here. */
}

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */

