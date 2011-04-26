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
#include "bu.h"
#include "NetMsgFactory.h"
#include "NetMsgTypes.h"
#include "NetMsgRouter.h"
#include "RemoteNodenameSetMsg.h"
#include "TypeOnlyMsg.h"
#include "RouteMsgJob.h"

Portal::Portal(PortalManager* pm, PkgTcpClient* client, struct pkg_switch* table):
pm(pm), pkgClient(client), callbackTable(table), log(Logger::getInstance()), handshakeComplete(false)
{
  GSUuid* uuid = GSUuid::createUuid();
  std::string str = uuid->toString();
  delete uuid;
  this->remoteNodeName.assign("NotSetYet-" + str);

  /* set the struct's userdata */
  this->callbackTable[0].pks_user_data = this;
}

Portal::~Portal() {
	delete callbackTable;
}

int
Portal::send(NetMsg* msg) {
  ByteBuffer* bb = msg->serialize();
  int retval = this->pkgClient->send(PKG_MAGIC2, bb->array(), bb->position());

  delete bb;

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
		GSUuid* uuid = GSUuid::createUuid();
		std::string str = uuid->toString();
		delete uuid;
		localNodeName.assign(str);
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
  Logger* log = Logger::getInstance();

  /* Check to see if we got a good Buffer and Portal Object */
  if (buf == 0) {
          log->logERROR("Portal", "pkg callback returned a NULL buffer!");
          /*	bu_bomb("pkg callback returned a NULL buffer!\n"); */
          return;
  }

  int len = conn->pkc_inend - sizeof(pkg_header);

  if(len < 1)
    return;

  ByteBuffer* bb = ByteBuffer::allocate(len);
  bb->put(buf, len);

  if (conn->pkc_user_data == 0) {
    log->logERROR("Portal", "pkg callback returned a NULL user_data pointer!");
    return;
  }

  Portal* p = (Portal*) conn->pkc_user_data;
  if (p == 0) {
      log->logERROR("Portal", "WARNING!  NULL Portal.");
  }

  /* Build a NetMsg */
  NetMsg* msg = NetMsgFactory::getInstance()->deserializeNetMsg(bb, p);
  if (msg == NULL) {
    log->logERROR("Portal", "WARNING!  NetMsg failed to deserialize properly.\n");
    return;
  }

  delete bb;

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

void
Portal::sendTypeOnlyMessage(uint32_t type, NetMsg* originalMsg)
{
	TypeOnlyMsg* tom = NULL;

	if (originalMsg == NULL)
		tom = new TypeOnlyMsg(type);
	else
		tom = new TypeOnlyMsg(type, originalMsg);

	this->send(tom);
	return;
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
