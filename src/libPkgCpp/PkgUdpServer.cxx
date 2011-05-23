/*                P K G U D P S E R V E R . C X X
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
/** @file PkgUdpServer.cxx
 * PkgUdpServer.cxx
 *
 *  Created on: Aug 19, 2010
 *
 */

#include "PkgUdpServer.h"

PkgUdpServer::PkgUdpServer() :
  PkgServer(PKGCPP_PROTO_UDP)
{}

PkgUdpServer::~PkgUdpServer()
{}

PkgClient*
PkgUdpServer::getNewClient(pkg_conn* conn)
{
  return new PkgUdpClient(conn);
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
