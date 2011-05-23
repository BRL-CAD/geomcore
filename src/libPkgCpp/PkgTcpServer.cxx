/*                P K G T C P S E R V E R . C X X
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
/** @file PkgTcpServer.cxx
 * PkgTcpServer.cxx
 *
 *  Created on: Aug 19, 2010
 *
 */

#include "PkgTcpServer.h"

PkgTcpServer::PkgTcpServer() :
	PkgServer(PKGCPP_PROTO_TCP) {}

PkgTcpServer::~PkgTcpServer() {}

PkgClient*
PkgTcpServer::getNewClient(pkg_conn* conn) {
	return new PkgTcpClient(conn);
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
