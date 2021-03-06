/*                        P O R T A L . H
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
/** @file Portal.h
 *
 * Brief description
 *
 */

#ifndef __PORTAL_H__
#define __PORTAL_H__

#define PKG_MAGIC2      0x5309

#include "INetMsgHandler.h"
#include "NetMsg.h"
#include "Logger.h"
#include "GSThread.h"
#include "pkg.h"
#include <string.h>

#define MAXCHUNKSIZE 4096

class PortalManager;
class MakeAndRouteMsgJob;

class Portal : public INetMsgHandler
{
public:
  friend class PortalManager;
  friend class MakeAndRouteMsgJob;

  virtual ~Portal();
  int send(NetMsg* msg);
  int sendThenDisconnect(NetMsg* msg);
  void sendTypeOnlyMessage(uint32_t type, NetMsg* originalMsg = 0);
  void sendGSNodeName();
  void disconnect();

  std::string getRemoteNodeName();
  bool handleNetMsg(NetMsg* msg);

  void tryBuild();

protected:
  Portal(PortalManager* pm, int socket);

/* Not for public use since libPKG will block on this call.
     * Returns:
     *          <0 on error
     *          0 on EOF
     *          1 on success
     */
  int pullFromSock();

private:
  PortalManager* pm;

  int socket;

  std::string remoteNodeName;
  Logger* log;
  bool handshakeComplete;

  GSMutex recvBufLock;
  ByteBuffer* recvBuffer;

  MakeAndRouteMsgJob* builder;

  void tryToBuildNetMsgs();
  bool routeNetMsg(NetMsg* msg);

  /* Disable copy cstr and =operator */
  Portal(Portal const&){};
  Portal& operator=(Portal const&){};
};

#endif /* __PORTAL_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
