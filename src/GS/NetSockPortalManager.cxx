/*        N E T S O C K P O R T A L M A N A G E R . C X X
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
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
/** @file NetSockPortalManager.cxx
 *
 * Brief description
 *
 */

#include "GS/NetSockPortalManager.h"
#include "GS/netMsg/RemHostNameSetMsg.h"
#include <QTcpSocket>
#include <iostream>

NetSockPortalManager::NetSockPortalManager(QString hostName, QObject* parent) :
	QTcpServer(parent), localHostName(hostName)
{
	this->log = Logger::getInstance();
	this->portalList = new QMap<QString, NetSockPortal*> ();
}

NetSockPortalManager::~NetSockPortalManager()
{
}

NetSockPortal* NetSockPortalManager::connectTo(QHostAddress addy, quint16 port)
{
	QString msg;
	msg += "Attempting to connect to: " + addy.toString() + ":" + port + "\n";
	this->log->log(Logger::INFO, msg);

	NetSockPortal* nsp = this->preparePortal();
	nsp->portStatus = NetSockPortal::NotConnected;

	//Wire up a signal to send the handshake info on connect
	QObject::connect(nsp, SIGNAL(connected()), this, SLOT(
			handleOutgoingConnect()));

	nsp->connectToHost(addy, port);

	return nsp;
}

NetSockPortal* NetSockPortalManager::preparePortal()
{
	//Create new NSP and setup signals
	NetSockPortal* nsp = new NetSockPortal(this);

	//Set up signal prior to initializing the NSP with a socket Descriptor
	QObject::connect(nsp, SIGNAL(portalHandshakeComplete()), this, SLOT(
			handlePortalHandshakeCompleted()));

	QObject::connect(nsp, SIGNAL(disconnect()), this, SLOT(
			handlePortalDisconnect()));
}

void NetSockPortalManager::incomingConnection(int socketDescriptor)
{
	NetSockPortal* nsp = this->preparePortal();

	nsp->setSocketDescriptor(socketDescriptor);
	nsp->portStatus = NetSockPortal::Handshaking;

	//Send the localhostName to the Remote machine.
	this->sendLocalHostName(nsp);

	emit newIncomingConnection(nsp);
}

void NetSockPortalManager::handleOutgoingConnect()
{
	NetSockPortal* nsp = (NetSockPortal*) sender();

	QString msg;
	msg += "Accepted new connection from: " + nsp->peerAddress().toString() + ":" + nsp->peerPort() + "\n";
	this->log->log(Logger::INFO, msg);

	//Send the localhostName to the Remote machine.
	this->sendLocalHostName(nsp);

	QObject::disconnect(nsp, SIGNAL(connected()), this, SLOT(
			handleOutgoingConnect(nsp)));

	emit newOutgoingConnection(nsp);
}

void NetSockPortalManager::handlePortalHandshakeCompleted()
{
	NetSockPortal* nsp = (NetSockPortal*) sender();

	//Map the NSP
	NetSockPortalManager::portalList->insert(nsp->getRemoteHostName(), nsp);

	QObject::disconnect(nsp, SIGNAL(portalHandshakeComplete()), this, SLOT(
			handlePortalHandshakeCompleted()));

	QString msg;
	msg += "Handshake with " + nsp->getRemoteHostName() + " completed.\n";
	this->log->log(Logger::INFO, msg);
}

void NetSockPortalManager::handlePortalDisconnect()
{
	NetSockPortal* nsp = (NetSockPortal*) sender();

	//Map the NSP
	NetSockPortalManager::portalList->remove(nsp->getRemoteHostName());
}

void NetSockPortalManager::sendLocalHostName(NetSockPortal* nsp)
{
	RemHostNameSetMsg msg(this->localHostName);
	nsp->send(msg);
}

NetSockPortal* NetSockPortalManager::getPortalByRemHostname(QString remHostName)
{
	return NetSockPortalManager::portalList->value(remHostName, NULL);
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// End:
// ex: shiftwidth=4 tabstop=8
