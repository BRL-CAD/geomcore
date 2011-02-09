/*                  P O R T A L . C X X
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
/** @file Portal.cxx
 *
 * Brief description
 *
 */

#include "Portal.h"
#include "PortalManager.h"
#include "Logger.h"
#include "brlcad/bu.h"
#include "NetMsgFactory.h"
#include "NetMsgTypes.h"
#include "NetMsgRouter.h"
#include "RemoteNodenameSetMsg.h"
#include "TypeOnlyMsg.h"
#include "RouteMsgJob.h"

Portal::Portal(PortalManager* pm, PkgTcpClient* client, struct pkg_switch* table):
pm(pm), pkgClient(client), callbackTable(table), log(Logger::getInstance()), handshakeComplete(false)
{
	this->remoteNodeName.assign("NotSetYet-" + QUuid::createUuid().toString().toStdString());

	/* set the struct's userdata */
	this->callbackTable[0].pks_user_data = this;
}

Portal::~Portal() {
	delete callbackTable;
}

int
Portal::send(NetMsg* msg) {
	QByteArray* ba = msg->serialize();
/*
	QString s("Sending msg.  Type: 0x");
	s.append(QString::number(msg->getMsgType(),16).toUpper());
	s.append(" len: ");
	s.append(QString::number(ba->size()));
	log->logDEBUG("Portal", s);
*/
	int retval = this->pkgClient->send(PKG_MAGIC2, ba->data(), ba->size());

	delete ba;

	/* Process any data moved by the underlying Socket buffer copy. */
	retval = this->pkgClient->processData();
	if (retval < 0) {
		this->log->logERROR("Portal",
				"Unable to process packets? Weird. (1) ");
		return retval;
	}/* TODO do we need to check for ==0 ? */

	return retval;
}
int
Portal::sendThenDisconnect(NetMsg* msg) {
	int retval = this->send(msg);

	/* TODO should we check to see if send actually sends first? */
	this->disconnect();

	return retval;
}

void
Portal::sendGSNodeName() {
	std::string localNodeName;
	localNodeName.assign(this->pm->getLocalNodeName());

	if (localNodeName.length() == 0) {
		localNodeName.assign(QUuid::createUuid().toString().toStdString());
	}

	RemoteNodenameSetMsg msg(localNodeName);
	this->send(&msg);
}

int
Portal::flush() {
	return this->pkgClient->flush();
}

int
Portal::read() {
	int retval = 0;

	/* recv first */
	retval = this->pkgClient->processData();
	if (retval < 0) {
		this->log->logERROR("Portal",
				"Unable to process packets? Weird. (1) ");
		return retval;
	}/* TODO do we need to check for ==0 ? */

	retval = this->pkgClient->pullDataFromSocket();
	if (retval < 0) {
		this->log->logERROR("Portal",
				"Seemed to have trouble pulling the data from the socket.");
		return retval;

	} else if (retval == 0) {
		this->log->logERROR("Portal", "Client closed the connection.");
		return retval;
	}

	retval = this->pkgClient->processData();
	if (retval < 0) {
		this->log->logERROR("Portal", "Unable to process packets? Weird. (2)");
		return retval;
	}/* TODO do we need to check for ==0 ? */

	return 1;
}

std::string
Portal::getRemoteNodeName() {
	return this->remoteNodeName + "";
}

bool
Portal::handleNetMsg(NetMsg* msg) {
	uint16_t type = msg->getMsgType();

	if (type == GS_REMOTE_NODENAME_SET) {
		if (this->handshakeComplete) {
			this->log->logDEBUG("Portal",
					"Recv-ed a RemoteNodename, but that is already set!");
		} else {
			RemoteNodenameSetMsg* t = (RemoteNodenameSetMsg*) msg;
			this->remoteNodeName.assign(t->getRemoteNodename());
			this->handshakeComplete = true;

			/*
			QString s("Recv-ed a RemoteNodename: ");
			s.append(this->remoteNodeName);
			this->log->logDEBUG("Portal", s);
			*/
		}

		/* Normally, the NetMsgRouter does the delete, but this opcode never gets routed. */
		delete msg;
		return true;

	} else if (type == RUALIVE) {
		TypeOnlyMsg tom(IMALIVE);
		this->send(&tom);
		delete msg;
		return true;
	}

	return false;
}

void
Portal::callbackSpringboard(struct pkg_conn* conn, char* buf) {
	Logger* log = 		Logger::getInstance();

	/* Check to see if we got a good Buffer and Portal Object */
	if (buf == 0) {
		log->logERROR("Portal", "pkg callback returned a NULL buffer!");
		/*	bu_bomb("pkg callback returned a NULL buffer!\n"); */
		return;
	}

	int len = conn->pkc_inend - sizeof(pkg_header);

	QByteArray ba(buf, len);

	if (conn->pkc_user_data == 0) {
		log->logERROR("Portal", "pkg callback returned a NULL user_data pointer!");
		return;
	}

	Portal* p = (Portal*) conn->pkc_user_data;

	if (p == 0) {
		log->logERROR("Portal", "WARNING!  NULL Portal.");
	}

	/* Build a NetMsg */
	NetMsg* msg = NetMsgFactory::getInstance()->deserializeNetMsg(ba, p);

	/* check to see if we deserialized the msg properly */
	if (msg == 0) {
		log->logERROR("Portal", "WARNING!  NetMsg failed to deserialize properly.\n");
		return;
	}

	/* Route */

	/* give the Portal first dibs on the netmsg */
	if (p->handleNetMsg(msg)) {
		return;
	}

	/* Fire off a Job.  This keeps the selector loop from */
	/* delivering all the Msg copies personally.*/
	RouteMsgJob* job = new RouteMsgJob(msg);
	job->submit();
}

void
Portal::disconnect()
{
	this->pm->disconnect(this);
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
