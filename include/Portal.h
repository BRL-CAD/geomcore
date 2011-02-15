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

#define PKG_MAGIC2      5309

#include "INetMsgHandler.h"
#include "PkgTcpClient.h"
#include "NetMsg.h"
#include "Logger.h"

#include "pkg.h"

#include <string>

class PortalManager;

class Portal : public INetMsgHandler
{
public:
  friend class PortalManager;
  virtual ~Portal();
  int send(NetMsg* msg);
  int sendThenDisconnect(NetMsg* msg);
  void sendGSNodeName();
  void disconnect();

  int flush();
  std::string getRemoteNodeName();
  bool handleNetMsg(NetMsg* msg);


protected:
  Portal(PortalManager* pm, PkgTcpClient* client, struct pkg_switch* table);


	/* Not for public use since libPKG will block on this call.
     * Returns:
     *          <0 on error
     *          0 on EOF
     *          1 on success
     */
  int read();

private:
  PortalManager* pm;
  struct pkg_switch* callbackTable;
  PkgTcpClient* pkgClient;
  std::string remoteNodeName;
  Logger* log;
  bool handshakeComplete;

  static void callbackSpringboard(struct pkg_conn* conn, char* buf);

	/* Disable copy cstr and =operator */
	Portal(Portal const&){};
	Portal& operator=(Portal const&){};
};

#endif /* __PORTAL_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
