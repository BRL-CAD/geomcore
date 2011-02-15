/*                    P K G C L I E N T . H
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
/** @file PkgClient.h
 *
 *
 */

#ifndef __PKGCLIENT_H__
#define __PKGCLIENT_H__

#include "pkgcppcommon.h"

#include "pkg.h"

#include <stdio.h>
#include <iostream>

class PkgClient
{
public:
  PkgClient(std::string proto, std::string ipOrHostname, int port, struct pkg_switch* callBackTableIn);
  PkgClient(std::string proto, pkg_conn* conn);
  virtual ~PkgClient();

  bool hasGoodConnection();

  /*
   * Blocks until a specific opcode has been received on the connection
   */
  char* waitForMsg(int opcode);

  /*
   * Attempts to route data in buffer to the appropriate call back.
   * Returns:
   *          <0 on error
   *          0 on EOF
   *          >0 on success
   */
  int processData();

  /*
   * Attempts to pull data off of the associated
   * socket and place it in the internal buffer
   * Returns:
   *          <0 on error
   *          0 on EOF
   *          >0 on success
   */
  int pullDataFromSocket();

  /**
   * Empty the stream buffer of any queued messages.
   *
   * Flush any pending data in the pkc_stream buffer.
   *
   * Returns < 0 on failure, else number of bytes sent.
   */
  int flush();

  void close();

  int send(int opcode, const char* buffer, size_t bufferLen);

  const struct pkg_switch* getCallBackTable();

  int getFileDescriptor();

private:
  std::string proto;
  pkg_conn* conn;

  /* Disable copy cstr and =operator */
  PkgClient(PkgClient const&){};
  PkgClient& operator=(PkgClient const&){};

};

#endif /* __PKGCLIENT_H__ */

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
