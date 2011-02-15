/*                  P K G S E R V E R . C X X
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
/** @file PkgServer.cxx
 *
 *
 */

#include "PkgServer.h"
#include "bu.h"
#include "pkg.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sstream>

PkgServer::PkgServer(std::string proto) {
	this->proto = proto;
	this->listenFD = -1;
}

PkgServer::~PkgServer() {}

int PkgServer::listen(unsigned short port, std::string ipOrHostname) {
	//Convert port -> char* to make libpkg happy.
	char portCString[7] = { 0 };
	int fd;
	snprintf(portCString, 6, "%d", port);

	fd = pkg_permserver_ip(ipOrHostname.c_str(), portCString, this->proto.c_str(), 0, 0);

	if (fd < 0)
		return fd;

	this->listenFD = fd;

	// bu_log("Listening on port '%d' (FD:%d).\n", port,fd);

	return fd;
}
int PkgServer::getListeningFD() {
	return this->listenFD;
}

/**
 * Attempts to open a new connection to ipOrHostname:port.  Returns NULL if connection fails for any reason.
 */
PkgClient*
PkgServer::connectToHost(std::string ipOrHostname, short port,
		struct pkg_switch* callbackTable) {
	std::stringstream ss;
	ss << port;
	std::string s_port = ss.str();

	pkg_conn* conn = pkg_open(ipOrHostname.c_str(), s_port.c_str(),
			this->proto.c_str(), NULL, NULL, callbackTable, NULL);

	if (conn == PKC_ERROR) {
		bu_log("Connection to %s, port %d, failed.\n", ipOrHostname.c_str(),
				port);
		return NULL;
	}
	return this->getNewClient(conn);
}

PkgClient*
PkgServer::waitForClient(struct pkg_switch* callbackTable, int waitTime) {
	pkg_conn* conn = pkg_getclient(this->listenFD, callbackTable, NULL,
			waitTime);

	if (conn == PKC_NULL) {
		if (waitTime == 0) {
			bu_log("Connection seems to be busy, waiting...\n");
			usleep(100);
		}
		return NULL;
	} else if (conn == PKC_ERROR) {
		//Fatal error accepting client connection
		bu_log("Fatal error accepting client connection.\n");
		pkg_close(conn);
		return NULL;
	}

	PkgClient* pkgClientObj = this->getNewClient(conn);
	return pkgClientObj;
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
