/*                    P K G C L I E N T . C X X
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
/** @file PkgClient.cxx
 *
 *
 */
#include "PkgClient.h"
#include "bu.h"
#include <string.h>

//TODO Need a path for a callback table to get to this
PkgClient::PkgClient(std::string proto, std::string ipOrHostname, int port, struct pkg_switch* callBackTableIn)
{
  this->proto = proto;
  char portCString[7] = { 0 };
  snprintf(portCString, 6, "%d", port);

  this->conn = pkg_open(ipOrHostname.c_str(), portCString, proto.c_str(), NULL,
      NULL, callBackTableIn, NULL);
}

PkgClient::PkgClient(std::string proto, pkg_conn* conn)
{
  this->proto = proto;
  this->conn = conn;
}

PkgClient::~PkgClient()
{}

bool
PkgClient::hasGoodConnection()
{
  return (this->conn != PKC_ERROR);
}

int
PkgClient::flush()
{
  //call the c function
  return pkg_flush(this->conn);
}

int
PkgClient::send(int opcode, const char* buffer, size_t bufferLen)
{
  //call the c function
  return pkg_send(opcode, buffer, bufferLen, this->conn);
}

void
PkgClient::close()
{
  //call the c function
  pkg_close(this->conn);
}

char*
PkgClient::waitForMsg(int opcode)
{
  char* buffer = pkg_bwaitfor(opcode, this->conn);
  if (buffer == NULL)
    {
      bu_log("Failed to process the client connection, still waiting\n");
      pkg_close(this->conn);
      this->conn = PKC_NULL;
    }

  return buffer;
}

int
PkgClient::processData()
{
  return pkg_process(this->conn);
}

int
PkgClient::pullDataFromSocket()
{
  int retVal = pkg_suckin(this->conn);
  return retVal;
}

const struct pkg_switch*
PkgClient::getCallBackTable()
{
	return this->conn->pkc_switch;
}

int
PkgClient::getFileDescriptor()
{
  return this->conn->pkc_fd;
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
