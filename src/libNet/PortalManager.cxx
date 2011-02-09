/*           P O R T A L M A N A G E R . C X X
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
/** @file PortalManager.cxx
 *
 * Brief description
 *
 */
#include "brlcad/bu.h"
#include "Portal.h"
#include "PortalManager.h"
#include "NetMsgFactory.h"
#include "PkgTcpClient.h"
#include "NetMsgTypes.h"

#include <string>
#include <stdio.h>
#include <errno.h>

#include <QtCore/QMap>

PortalManager::PortalManager(std::string localNodeName, uint16_t listenPort, std::string address)
{
	this->localNodeName.assign(localNodeName + "PortMan");
	this->listenAddress.assign(address);
	this->listenPort = listenPort;
	this->tcpServer = new PkgTcpServer();
	this->fdPortalMap = new QMap<int, Portal*> ();
	this->portalsLock = new QMutex();
	this->log = Logger::getInstance();
	this->fdmax = 0;
}

PortalManager::~PortalManager() {}

Portal*
PortalManager::connectToHost(std::string host, uint16_t port) {
	struct pkg_switch* table = this->makeNewSwitchTable();

	PkgTcpClient* pkgc = (PkgTcpClient*) this->tcpServer->connectToHost(host, port, table);

	if (pkgc == NULL) {
		return NULL;
	} else {
		Portal* p = this->makeNewPortal(pkgc, table);
		return p;
	}
}

void
PortalManager::_run() {
	this->log->logINFO("PortalManager", "Running");
	struct timeval timeout;
	fd_set readfds;
	fd_set exceptionfds;
	int listener = -1;

	this->masterFDSLock.lock();
	FD_ZERO(&masterfds);
	this->masterFDSLock.unlock();

	FD_ZERO(&readfds);
	FD_ZERO(&exceptionfds);

	if (this->listenPort != 0) {
		listener = this->tcpServer->listen(this->listenPort, this->listenAddress);

		if (listener < 0) {
			this->log->logERROR("PortalManager", "Failed to listen");
			return;
		} else {
			char buf[BUFSIZ];
			std::string s;
			snprintf(buf, BUFSIZ, "%s:%d FD:%d", this->listenAddress.c_str(), this->listenPort, listener);
			s.assign(buf);
			this->log->logINFO("PortalManager", s);
		}

		this->masterFDSLock.lock();
		FD_SET(listener, &masterfds);
		fdmax = listener;
		this->masterFDSLock.unlock();
	}

	bool isListener = false;
	bool readyRead = false;
	bool readyAccept = false;
	bool readyException = false;

	while (this->getRunCmd()) {
		/* Set values EVERY loop since select() on *nix modifies this. */
		timeout.tv_sec = 0;
		timeout.tv_usec = 50 * 1000;

		this->masterFDSLock.lock();
		readfds = masterfds;
		exceptionfds = masterfds;
		this->masterFDSLock.unlock();

		/* Shelect!! */
		int retVal = select(fdmax + 1, &readfds, NULL, &exceptionfds, &timeout);

		/*
		if (retVal != 0) {
			QString out("Select returned: ");
			out.append(QString::number(retVal));
			out.append(". FD count: ");
			out.append(QString::number(this->fdPortalMap->keys().size()));
			out.append(". MAX FD: ");
			out.append(QString::number(fdmax));
			this->log->logINFO("PortalManager", out);
		}
	*/

		if (retVal < 0) {
			/* got a selector error */
			this->log->logERROR("PortalManager", "Selector Error: " + QString::number(errno).toStdString());
			break;
		}

		for (int i = 0; i <= fdmax; ++i) {
			bool isaFD = FD_ISSET(i, &masterfds);

			/* Don't muck with an FD that isn't ours! */
			if (!isaFD) {
				continue;
			}

			/* Simplify switching later with bools now */
			isListener = (i == listener);
 			readyRead =  FD_ISSET(i, &readfds) && !isListener;
			readyAccept = FD_ISSET(i, &readfds) && isListener;
			readyException = FD_ISSET(i, &exceptionfds);

			/* If nothing to do, then continue; */
			if (!readyRead && !readyAccept && !readyException) {
				continue;
			}

			/* Handle exceptions */
			if (readyException) {
				/* TODO handle exceptions */
				perror("Exception on FileDescriptor");
			}

			Portal* p = NULL;
			/* Accept new connections: */
			if (readyAccept) {
				struct pkg_switch* table = this->makeNewSwitchTable();

				PkgTcpClient* client =
						(PkgTcpClient*) this->tcpServer->waitForClient(table,
								42);

				if (client == 0) {
					log->logERROR("PortalManager",
							"Error on accepting new client.");
				} else {
					/* Handle new client here. */
					p = this->makeNewPortal(client, table);
				}
			}

			/* the only thing we want to do on the listener loop is accept */
			if (isListener) {
				continue;
			}

			/* If we didnt get a portal from accepting, then get one from the map */
			if (p == 0 && this->fdPortalMap->contains(i)) {
				this->portalsLock->lock();
				p = this->fdPortalMap->value(i);
				this->portalsLock->unlock();
			}

			/* Check, again, if we have a good portal. */
			if (p == 0) {
				/* Deal with unmapped file Descriptor */
				char buf[BUFSIZ];
				snprintf(buf, BUFSIZ, "FD %d not associated with a Portal, dropping connection.");
				std::string s(buf);
				this->closeFD(i, s);
				continue;
			}

			/* read */
			if (readyRead) {
				int readResult = p->read();

				if (readResult == 0) {
					this->closeFD(i, "");
					continue;
				} else if (readResult < 0) {
					this->closeFD(i, "Error on read, dropping connection.");
					continue;
				}
			}
		} /* end FOR */
	} /* end while */
	this->log->logINFO("PortalManager", "Shutdown");
}/* end fn */

Portal*
PortalManager::makeNewPortal(PkgTcpClient* client, struct pkg_switch* table) {
	Portal* p = new Portal(this, client, table);

	if (p == 0) {
		return 0;
	}

	/* Obtain lock and then map this new portal */
	this->portalsLock->lock();
	int newFD = p->pkgClient->getFileDescriptor();
	this->fdPortalMap->insert(newFD, p);
	this->portalsLock->unlock();
/*
	QString s("New Portal with FD: ");
	s.append(QString::number(newFD));
	log->logDEBUG("PortalManager", s);
*/

	/* Check maxFD and update if needed. */
	if (newFD > fdmax) {
		fdmax = newFD;
	}

	/* Add to masterFDS. */
	this->masterFDSLock.lock();
	FD_SET(newFD, &masterfds);
	this->masterFDSLock.unlock();

	p->sendGSNodeName();

	return p;
}

struct pkg_switch*
PortalManager::makeNewSwitchTable() {
	struct pkg_switch* table = new pkg_switch[2];

	table[0].pks_type = PKG_MAGIC2;
	table[0].pks_handler = &(Portal::callbackSpringboard);
	table[0].pks_title = "SpringBoard";
	table[0].pks_user_data = 0;

	table[1].pks_type = 0;
	table[1].pks_handler = 0;
	table[1].pks_title = (char*) 0;
	table[1].pks_user_data = 0;

	return table;
}

void
PortalManager::closeFD(int fd, std::string logComment) {
	close(fd);

	this->masterFDSLock.lock();
	if (FD_ISSET(fd, &this->masterfds)) {
		FD_CLR(fd, &this->masterfds);
	}
	this->masterFDSLock.unlock();

	this->portalsLock->lock();
	this->fdPortalMap->remove(fd);
	this->portalsLock->unlock();

	if (logComment.length() >0) {
		this->log->logINFO("PortalManager", logComment);
	}
}

void
PortalManager::disconnect(Portal* p)
{
	int fd = p->pkgClient->getFileDescriptor();
	this->closeFD(fd, "Disconnect requested.");
}

bool
PortalManager::handleNetMsg(NetMsg* msg)
{
	uint16_t type = msg->getMsgType();
	switch(type) {
	case DISCONNECTREQ:
		this->handleDisconnectReqMsg((TypeOnlyMsg*)msg);
		return true;
	}
	return false;
}

void
PortalManager::handleDisconnectReqMsg(TypeOnlyMsg* msg)
{
	Portal* origin = msg->getOrigin();

	/* validate incoming data */
	if (origin == 0) {
		/* TODO Figure out how to how to handle NULL Portal */
		log->logERROR("PortalManager", "handleDisconnectReqMsg(): NULL Portal!");
		return;
	}

	this->disconnect(origin);
}

std::string
PortalManager::getLocalNodeName()
{
	return this->localNodeName;
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
