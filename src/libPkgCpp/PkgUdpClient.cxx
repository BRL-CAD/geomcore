/*                P K G U D P C L I E N T . C X X
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
/** @file PkgUdpClient.cxx
 * PkgUdpClient.cxx
 *
 *  Created on: Aug 19, 2010
 *
 */

#include "PkgUdpClient.h"

PkgUdpClient::PkgUdpClient(std::string ipOrHostname, int port, struct pkg_switch* callBackTableIn) :
  PkgClient(PKGCPP_PROTO_UDP, ipOrHostname, port, callBackTableIn)
{}

PkgUdpClient::PkgUdpClient(pkg_conn* conn) :
  PkgClient(PKGCPP_PROTO_UDP, conn)
{}

PkgUdpClient::~PkgUdpClient()
{}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
