/*                  P K G U D P C L I E N T . H
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
/** @file PkgUdpClient.h
 * PkgUdpClient.h
 *
 *  Created on: Aug 19, 2010
 *
 */

#ifndef __PKGUDPCLIENT_H__
#define __PKGUDPCLIENT_H__

#include "PkgClient.h"

class PkgUdpClient : public PkgClient
{
public:
  PkgUdpClient(std::string ipOrHostname, int port, struct pkg_switch* callBackTableIn);
  PkgUdpClient(pkg_conn* conn);
  virtual ~PkgUdpClient();

private:
	/* Disable copy cstr and =operator */
	PkgUdpClient(PkgUdpClient const&):PkgClient("",NULL){};
	PkgUdpClient& operator=(PkgUdpClient const&){};
};

#endif /* __PKGUDPCLIENT_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
