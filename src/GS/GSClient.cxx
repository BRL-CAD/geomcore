/*                    G S C L I E N T . C X X
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
/** @file GSClient.cxx
 *
 */
#include "raytrace.h"

#include "GSClient.h"
#include "Portal.h"
#include "NetMsgRouter.h"
#include "SessionInfoMsg.h"
#include "FailureMsg.h"
#include "PongMsg.h"
#include "GeometryManifestMsg.h"
#include "GeometryChunkMsg.h"
#include "DirListResMsg.h"


GSClient::GSClient(std::string localNodeName)
{
    this->log = Logger::getInstance();
    this->jobMan = JobManager::getInstance();
    this->jobMan->startup();

    this->portMan = new PortalManager(localNodeName);
    this->portMan->start();
    usleep(100 * 1000);

    this->registerMsgRoutes();
}

GSClient::~GSClient()
{
    delete this->portMan;
}

void
GSClient::registerMsgRoutes()
{
    NetMsgRouter* router = NetMsgRouter::getInstance();

    router->registerType(DISCONNECTREQ, this->portMan);
    router->registerType(SESSIONINFO, this);
    router->registerType(FAILURE, this);
    router->registerType(PING, this);
    router->registerType(PONG, this);
    router->registerType(GEOMETRYMANIFEST, this);
    router->registerType(GEOMETRYCHUNK, this);
    router->registerType(DIRLISTREQ, this);
    router->registerType(DIRLISTRES, this);
}

bool
GSClient::handleNetMsg(NetMsg* msg)
{
  uint16_t type = msg->getMsgType();
  char buf[BUFSIZ];

  switch (type)
    {
  case SESSIONINFO:
    {
      std::string data = ((SessionInfoMsg*) msg)->toString();
      log->logINFO("GSClient", "Recv'ed SessionInfo: " + data);
      return true;
    }
  case FAILURE:
    {
      FailureMsg* fMsg = (FailureMsg*) msg;
      uint8_t fc = fMsg->getFailureCode();

      GSUuid* re = fMsg->getReUUID();

      snprintf(buf, BUFSIZ, "Recv'ed A FailureMsg with code: %d (%x)", fc, fc);
      log->logINFO("GSClient", buf);
      return true;
    }
  case PING:
    {
      Portal* p = msg->getOrigin();
      PingMsg* pingMsg = (PingMsg*) msg;

      std::stringstream ss;

      std::string remNodeName("unknown");
      if (p != NULL)
        remNodeName = p->getRemoteNodeName();

      ss << "PING from: '" << remNodeName << "' ";
      ss << "Start Time: " << pingMsg->getStartTime();

      log->logINFO("GeometryService", ss.str());

      if (p != NULL)
        {
          PongMsg pongMsg((PingMsg*) msg);
          p->send(&pongMsg);
        }
      else
        {
          log->logINFO("GeometryService", "Can't return ping.  NULL Portal*");
        }

      return true;
    }
  case PONG:
    {
      Portal* p = msg->getOrigin();
      PongMsg* pongMsg = (PongMsg*) msg;

      /* calc current and differential times */
      uint64_t start = pongMsg->getStartTime();
      uint64_t now = Logger::getCurrentTime();
      uint64_t diff = now - start;

      std::string remNodeName("unknown");

      if (p != NULL)
        remNodeName = p->getRemoteNodeName();

      std::stringstream ss;
      ss << "PONG from: '" << remNodeName << "' ";
      ss << " Start: " << start;
      ss << " Now: " << now;
      ss << " Diff: " << diff;

      log->logINFO("GSClient", ss.str());
      return true;
    }

  case GEOMETRYMANIFEST:
    {
      GeometryManifestMsg* man = (GeometryManifestMsg*) msg;

      std::string str;
      std::stringstream ss;
      ss << "Got manifest of " << man->getNumOfItems() << " items.";
      log->logINFO("GSClient", ss.str());

      return true;
    }
  case GEOMETRYCHUNK:
    {
      GeometryChunkMsg* chunk = (GeometryChunkMsg*) msg;
      std::stringstream ss;
      ss << "Processing chunk: " << chunk->getPath();
      log->logINFO("GSClient", ss.str());

      return true;
    }
  case DIRLISTREQ:
    {
      Portal* p = msg->getOrigin();
      std::string remNodeName("unknown");

      if (p != NULL)
        remNodeName = p->getRemoteNodeName();

       log->logINFO("GSClient", "Got a directory list *REQUEST* (?) from " + remNodeName);

      return true;
    }
  case DIRLISTRES:
    {
      Portal* p = msg->getOrigin();
      std::string remNodeName("unknown");

      if (p != NULL)
        remNodeName = p->getRemoteNodeName();

       DirListResMsg* dir = (DirListResMsg*)msg;
       std::list<std::string> items;
       std::list<std::string>::iterator it;
       std::cout << "\nListing for: " << dir->getPath() << "\n";
       dir->getItems(&items);
       it = items.begin();

       if (items.size() == 0)
         std::cout << "\t -- EMPTY -- \n";

       for(;it != items.end();++it)
           std::cout << "\t" << (*it) << "\n";

       std::cout << std::endl;
      return true;
    }
    }
  return false;
}

PortalManager*
GSClient::getPortMan()
{
    return this->portMan;
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
