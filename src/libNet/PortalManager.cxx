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
#include "bu.h"
#include "Portal.h"
#include "PortalManager.h"
#include "NetMsgFactory.h"
#include "NetMsgTypes.h"
#include "JobManager.h"
#include "MakeAndRouteMsgJob.h"

#include <string>
#include <stdio.h>
#include <errno.h>

#include <map>

PortalManager::PortalManager(std::string localNodeName, uint16_t listenPort, std::string address)
{
	this->localNodeName.assign(localNodeName);
	this->listenAddress.assign(address);
	this->listenPort = listenPort;
	this->fdPortalMap = new std::map<int, Portal*> ();
	this->portalsLock = new GSMutex();
	this->log = Logger::getInstance();
	this->fdmax = 0;
}

PortalManager::~PortalManager() {}

Portal*
PortalManager::connectToHost(std::string host, uint16_t port) {

//  struct pkg_switch* table = new pkg_switch[1];
//
//  table[0].pks_type = 0;
//  table[0].pks_handler = 0;
//  table[0].pks_title = (char*) 0;
//  table[0].pks_user_data = 0;

  std::stringstream ss;
  ss << port;
  std::string s_port = ss.str();

  /* Use PKG to do all the cross platform stuff */
  pkg_conn* conn = pkg_open(host.c_str(), s_port.c_str(),
      "tcp", NULL, NULL, NULL, NULL);

    if (conn == PKC_ERROR) {
            bu_log("Connection to %s, port %d, failed.\n", host.c_str(),
                            port);
            return NULL;
    }

    Portal* p = this->makeNewPortal(conn->pkc_fd);

    /* Dunno if i can do this without killing the FD */
    free(conn);

    return p;
}

int
PortalManager::listen()
{
  std::stringstream ss;
  ss << this->listenPort;
  std::string s_port = ss.str();
  int fd = pkg_permserver_ip(this->listenAddress.c_str(), s_port.c_str(), "tcp", 0, 0);
  return fd;
}

int
PortalManager::accept(int listener)
{
  /* Use PKG to do all the cross platform stuff */
  pkg_conn* conn = pkg_getclient(listener, NULL, NULL, 42);

  if (conn == NULL) {
      return -1;
  } else if (conn == PKC_ERROR) {
      bu_log("Fatal error accepting client connection.\n");
      free(conn);
      return -1;
  }

  int fd = conn->pkc_fd;
  free(conn);
  return fd;
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

  //TODO eventually make this listen on unlimited number of ports.
  /* Setup listening on single port */
  if (this->listenPort != 0) {
    listener = this->listen();

    if (listener < 0) {
        this->log->logERROR("PortalManager", "Failed to listen");
        return;
    } else {
        std::stringstream ss;
        ss << "Listening on " << this->listenAddress;
        ss << ":" << (int)this->listenPort;
        ss << " FD:" << (int)listener;
        this->log->logINFO("PortalManager", ss.str());
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
  int newFD = 0;

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

      if (retVal < 0) {
          char buf[BUFSIZ];
          /* got a selector error */
          snprintf(buf, BUFSIZ, "Selector Error: %d", errno);
          this->log->logERROR("PortalManager", buf);
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
              newFD = this->accept(listener);

              if (newFD < 1) {
                  log->logERROR("PortalManager",
                      "Error on accepting new client.");
              } else {
                  /* Handle new client here. */
                  p = this->makeNewPortal(newFD);
              }
          }

          /* the only thing we want to do on the listener loop is accept */
          if (isListener) {
              continue;
          }

          /* If we didnt get a portal from accepting, then get one from the map */
          if (p == 0 && (*this->fdPortalMap)[i]) {
              this->portalsLock->lock();
              p = (*this->fdPortalMap)[i];
              this->portalsLock->unlock();
          }

          /* Check, again, if we have a good portal. */
          if (p == 0) {
              /* Deal with unmapped file Descriptor */
              char buf[BUFSIZ];
              snprintf(buf, BUFSIZ, "FD %d not associated with a Portal, dropping connection.", i);
              std::string s(buf);
              this->closeFD(i, s);
              continue;
          }

          /* read */
          if (readyRead) {
//              std::cout << "\nCalling pullFromSock()\n";
              int readResult = p->pullFromSock();
//              std::cout << "\nDone Calling pullFromSock(), readResult was: " << readResult << "\n";

//              MakeAndRouteMsgJob* j = new MakeAndRouteMsgJob(p);
//              JobManager::getInstance()->submitJob(j);

              p->tryBuild();

              if (readResult == 0) {
                  this->closeFD(i, "Closing FD (read returned zero)");
                  continue;
              } else if (readResult < 0) {
                  this->closeFD(i, "Error on read, dropping connection(254).");
                  continue;
              }
          }
      } /* end FOR */
  } /* end while */
  this->log->logINFO("PortalManager", "Shutdown");
}/* end fn */

Portal*
PortalManager::makeNewPortal(int socket) {
  Portal* p = new Portal(this, socket);

  /* Obtain lock and then map this new portal */
  this->portalsLock->lock();
  this->fdPortalMap->insert(std::pair<int,Portal*>(socket, p));
  this->portalsLock->unlock();

  /* Check maxFD and update if needed. */
  if (socket > fdmax)
    fdmax = socket;

  /* Add to masterFDS. */
  this->masterFDSLock.lock();
  FD_SET(socket, &masterfds);
  this->masterFDSLock.unlock();

  /* Start handshake */
  p->sendGSNodeName();

  return p;
}

void
PortalManager::closeFD(int fd, std::string logComment)
{
  close(fd);

  this->masterFDSLock.lock();
  if (FD_ISSET(fd, &this->masterfds)) {
      FD_CLR(fd, &this->masterfds);
  }
  this->masterFDSLock.unlock();

  this->portalsLock->lock();
  this->fdPortalMap->erase(fd);
  this->portalsLock->unlock();

  if (logComment.length() > 0)
    this->log->logINFO("PortalManager", logComment);
}

void
PortalManager::disconnect(Portal* p)
{
  this->closeFD(p->socket, "Disconnect requested.");
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

std::string
PortalManager::getPortalManagerName()
{
  std::string out = this->localNodeName;
  out += "PortMan";
  return out;
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
