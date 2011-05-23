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

#include <unistd.h>
#include <errno.h>
#include "Portal.h"
#include "PortalManager.h"
#include "Logger.h"
#include "bu.h"
#include "NetMsgFactory.h"
#include "NetMsgTypes.h"
#include "NetMsgRouter.h"
#include "RemoteNodenameSetMsg.h"
#include "TypeOnlyMsg.h"
#include "JobManager.h"
#include "MakeAndRouteMsgJob.h"

Portal::Portal(PortalManager* pm, int socket):
pm(pm), log(Logger::getInstance()), handshakeComplete(false),
socket(socket)
{
  GSUuid* uuid = GSUuid::createUuid();
  std::string str = uuid->toString();
  delete uuid;
  this->remoteNodeName.assign("NotSetYet-" + str);

  /* set an 256K initial size. */
  this->recvBuffer = ByteBuffer::allocate((1024 * 256));
  this->builder = new MakeAndRouteMsgJob(this);
}

Portal::~Portal() {
  delete this->builder;
}

int
Portal::send(NetMsg* msg) {
  ByteBuffer* bb = msg->serialize();
  int len = bb->position();
  int retval = 0;

  int pos = 0;
  int loopCnt = 0;
  int totalSend = 0;
  int loopToSend = 0;
  while (pos < len) {
      if (pos + MAXCHUNKSIZE > len)
        loopToSend = (len-pos);
      else
        loopToSend = MAXCHUNKSIZE;

#ifdef HAVE_WINSOCK_H
      retval = send(this->socket, bb->array() + pos, loopToSend);
#else
      retval = write(this->socket, bb->array() + pos, loopToSend);
#endif

      if (retval == -1)
        //error occurred
        break;

      if (retval == 0) {
        usleep(10);
        continue;
      }

      totalSend += retval;
      ++loopCnt;
      pos += retval;

      usleep(10);

/*
      std::cout << "(looped: "<< loopCnt <<") Wanted to send: " << loopToSend ;
      std::cout << ", actually sent: " << retval;
      std::cout << ", Total sent: " << totalSend ;
      std::cout << ", Total TO BE sent: " << len << "\n";
*/

  }
  if (retval == -1){
    std::cout << "Incomplete send: " << totalSend << " bytes.\n";
    std::cout <<
        "Seemed to have trouble writing to the socket: "
      << errno << " ("<< strerror( errno ) << ")\n";

  }
//  else
//    std::cout << "Finished send: " << totalSend << " bytes.\n";

  delete bb;
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
Portal::pullFromSock() {
  int lastRead = 0;
  int tryToRead = MAXCHUNKSIZE;
  int remaining = 0;
  int pos = 0;
  int cap = 0;
  int totalRead = 0;

  /* Transfer data to portal's RecvBuffer */
  /* Assume that recvBuf->position is at the place where we need to start writing. */
  GSMutexLocker locker(&this->recvBufLock);
  ByteBuffer* bb = this->recvBuffer;

  int testLoopLimiter = 0;

   do {
      if (bb->remaining() < 4){
          /* Force a resize */
          bb->put64bit(0);
          bb->put64bit(0);
          bb->put64bit(0);
          bb->setPosition(bb->position() - 24);
          testLoopLimiter += 20;
      }

      /* set limit to capacity for safety */
      cap = bb->capacity();
      bb->setLimit(cap);
      pos = bb->position();

      /* Check for enough space in BB */
      remaining = cap - pos;

      if (remaining < MAXCHUNKSIZE)
         tryToRead = remaining - 1;
      else
         tryToRead = MAXCHUNKSIZE;

//      std::cout << " pos: " << pos ;
//      std::cout << " cap: " << cap ;
//      std::cout << " remaining: " << remaining ;
//      std::cout << " tryToRead: " << tryToRead ;
//      std::cout << " pos + tryToRead: " << (pos + tryToRead) << "\n" ;

#ifdef HAVE_WINSOCK_H
      lastRead = recv(this->socket, bb->array() + pos, tryToRead);
#else
      lastRead = read(this->socket, bb->array() + pos, tryToRead);
#endif

      if (lastRead < 0){
          this->log->logERROR("Portal",
          "Seemed to have trouble pulling the data from the socket.");
          std::cout <<
          "Seemed to have trouble pulling the data from the socket: "
              << errno << " ("<< strerror( errno ) << ")\n";

      } else {
        pos += lastRead;
        totalRead += lastRead;
        bb->setPosition(pos);
      }
//      std::cout << "read() returned: " << lastRead << "\n";

      /* Break out if read comes back less than MAXED */
      if (lastRead < MAXCHUNKSIZE)
        break;

      ++testLoopLimiter;
      if (testLoopLimiter > 10)
        break;

   } while (lastRead > 0);

   /* Clamp the return val */
   int retVal = 0;
   if (lastRead > 0)
     retVal  = 1;
   if (lastRead < 0)
     retVal =  -1;

//   std::cout << "pullFromSock() loop exit. totalRead: " << totalRead;
//   std::cout << " retVal: " << retVal << "\n";

  return retVal;
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

void
Portal::tryBuild()
{
  if (this->builder->getStatus() == JOB_RUNNING)
    return;

  JobManager::getInstance()->submitJob(this->builder);
}

void
Portal::tryToBuildNetMsgs() {
  GSMutexLocker locker(&this->recvBufLock);
  ByteBuffer* bb = this->recvBuffer;
//  std::cout << "Portal's buffer position after data addition: " << bb->position() << "\n";

//  std::cout << "\nByteDump: " << bb->toHexString() << "\n\n";

  NetMsg* msg = NULL;
  int start = 0;
  int endOfData = bb->position();
  bb->flip();
  while (msg == NULL)
    {
      if (bb->remaining() == 0) {
//          std::cout << "No data left\n";
          break;
      }

      start = bb->position();

      /* read GS header */
      uint16_t msgType = bb->get16bit();
      uint32_t msgLen = bb->get32bit();
      bb->setPosition(bb->position() - 6);

      if (bb->remaining() < msgLen) {
//          std::cout << "Not enough data to build the next message: type=" << msgType;
//          std::cout << " requiredLen: " << msgLen;
//          std::cout << " have: " << bb->remaining();
//          std::cout << " currentPos: " << bb->position();
//          std::cout << " capacity: " << bb->capacity() << "\n";
          bb->setPosition(start);
          bb->setLimit(endOfData);
          break;
      }

      /* Build a NetMsg */
//      std::cout << "starting build at position: " << bb->position() << "\n";
      NetMsg* msg = NetMsgFactory::getInstance()->deserializeNetMsg(bb, this);
      if (msg == NULL) {
          log->logERROR("Portal",
              "WARNING!  NetMsg failed to deserialize properly.\n");
//          std::cout << "build failed at position: " << bb->position() << "\n";
          bb->setPosition(start);
          bb->setLimit(endOfData);
          break;
        }
//      std::cout << "build succeeded at position: " << bb->position() << "\n";

      /* Route */
      if (Portal::routeNetMsg(msg) == false) {
          std::stringstream ss;
          ss << "WARNING!  Failed to find route for NetMsg type: " << msg->getMsgType() << "\n";
          log->logERROR("Portal", ss.str());
          delete msg;
        }

      /* Set back to NULL for proper loop logic */
      msg == NULL;
    }
 /* Compact BB */
  bb->compact();
}

bool
Portal::routeNetMsg(NetMsg* msg) {
  /* give the Portal first dibs on the netmsg */
  if (msg->getOrigin()->handleNetMsg(msg) == true)
    return true;

  return NetMsgRouter::getInstance()->routeMsg(msg);
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
