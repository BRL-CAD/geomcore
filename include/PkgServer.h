/*                    P K G S E V E R . H
 * BRL-CAD
 *
 * Copyright (c) 2004-2011 United States Government as represented by
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
/** @file PkgServer.h
 *
 *
 */

#ifndef __PKGSERVER_H__
#define __PKGSERVER_H__

#include "pkg.h"
#include "pkgcppcommon.h"
#include "PkgClient.h"

class PkgServer
{
public:
  PkgServer(std::string proto);
  virtual ~PkgServer();

  int listen(unsigned short port, std::string ipOrHostname = "localhost");
  PkgClient* connectToHost(std::string ipOrHostname, short port, struct pkg_switch* callbackTable);

  /*
   * This function blocks if waitTime is set to '0'.  '0' is default.
   */
  PkgClient*  waitForClient(struct pkg_switch* callbackTable, int waitTime = 0);
  int getListeningFD();

private:
  std::string proto;
  int listenFD;

  virtual PkgClient*  getNewClient(pkg_conn* conn) = 0;

	/* Disable copy cstr and =operator */
	PkgServer(PkgServer const&){};
	PkgServer& operator=(PkgServer const&){};

};

#endif /* __PKGSERVER_H__ */

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */

