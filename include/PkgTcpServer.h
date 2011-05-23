/*                  P K G T C P S E R V E R . H
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
/** @file PkgTcpServer.h
 * PkgTcpServer.h
 *
 *  Created on: Aug 19, 2010
 *
 */

#ifndef __PKGTCPSERVER_H__
#define __PKGTCPSERVER_H__

#include "PkgServer.h"
#include "PkgTcpClient.h"

class PkgTcpServer : public PkgServer
{
public:
  PkgTcpServer();
  virtual ~PkgTcpServer();

private:
  PkgClient* getNewClient(pkg_conn* conn);

	/* Disable copy cstr and =operator */
	PkgTcpServer(PkgTcpServer const&):PkgServer(""){};
	PkgTcpServer& operator=(PkgTcpServer const&){};
};

#endif /* __PKGTCPSERVER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
