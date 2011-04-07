/*                  P K G C P P T E S T . C X X
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
/** @file pkgcppTest.cxx
 *
 * Brief description
 *
 */

#include "bu.h"
#include "pkg.h"

#include "PkgTcpServer.h"
#include "PkgTcpClient.h"
#include "Logger.h"

#include <stdio.h>
#include <iostream>
#include <algorithm>
#include <cstring>

#define SERVER_NAME "ImaServer"
#define CLIENT_NAME "ImaClient"
#define ENET_PROTO "tcp"
#define RAND_DATA_SIZE 1024

/* simple network transport protocol. connection starts with a HELO,
 * then a variable number of GEOM/ARGS messages, then a CIAO to end.
 */

#define PKGCPP_HELO        1
#define PKGCPP_DATA        2
#define PKGCPP_CIAO        3

//Declares
void
printUsage(std::string customMsg);
std::string
getValidIpOrHostname(char* data);
int
getValidPort(char* data);
int
tryProcess(PkgClient* pkgClient);
int
runServer(int port);
int
runClient(std::string ipOrHostname, int port);

void
server_helo(struct pkg_conn* c, char* buf);
void
server_data(struct pkg_conn* c, char* buf);
void
server_ciao(struct pkg_conn* c, char* buf);

//global
bool listenLoop = true;

int
main(int argc, char** argv)
{
  if (argc > 4 || argc < 3)
    {
      printUsage("Incorrect ARG count.");
      bu_exit(1, "");
    }

  bool isServer;

  //Get app mode.  Either client or server
  std::string cliServ(argv[1]);
  std::transform(cliServ.begin(), cliServ.end(), cliServ.begin(), tolower);

  if (cliServ == "client")
    {
      isServer = false;
    }
  else if (cliServ == "server")
    {
      isServer = true;
    }
  else
    {
      printUsage("Unknown mode: '" + cliServ + "'");
      bu_exit(1, "");
    }

  int exitVal = 0;
  if (isServer)
    {
      int port = getValidPort(argv[2]);
      exitVal = runServer(port);

    }
  else
    {
      std::string ipOrHostname = getValidIpOrHostname(argv[2]);
      int port = getValidPort(argv[3]);
      exitVal = runClient(ipOrHostname, port);
    }

  return exitVal;
}

/**
 * Converts char* data to a std::string that represents an IP or a hostname
 */
std::string
getValidIpOrHostname(char* data)
{
  //Check IP/Host
  std::string ipOrHostname(data);

  if (ipOrHostname.length() > 0)
    {
      //More validation goes here, if needed.
    }
  else
    {
      printUsage("Supplied IP/Host: '" + ipOrHostname + "' is invalid");
      bu_exit(1, "");
    }

  return ipOrHostname;
}

/**
 * Converts char* data to a valid port number
 */
int
getValidPort(char* data)
{
  std::string portStr(data);
  int port = atoi(data);

  //Hardcode prolly not best for OS determined port range....
  if (port > 0x0000 && port < 0xFFFF)
    {
      //More validation goes here, if needed.
    }
  else
    {
      printUsage("Supplied Port '" + portStr + "' is invalid.");
      bu_exit(1, "");
    }
  return port;
}

int
runServer(int port)
{
  struct pkg_switch callbacks[] =
    {
      { PKGCPP_HELO, server_helo,  (char*)"HELO" },
      { PKGCPP_DATA, server_data,  (char*)"DATA" },
      { PKGCPP_CIAO, server_ciao,  (char*)"CIAO" },
      { 0, 0, (char*) 0 } };

  PkgTcpServer pkgServer;
  pkgServer.listen(port);

  //Setup vars
  char* buffer;

  PkgClient* pkgClient = NULL;

  /* listen for a good client indefinitely.  this is a simple
   * handshake that waits for a HELO message from the client.  if it
   * doesn't get one, the server continues to wait.
   */
  do
    {
      //Blocks
      pkgClient = pkgServer.waitForClient(callbacks);

      //TODO probably should failsafe this loop
      if (pkgClient == NULL)
        continue;

      /*      //Attempt to get a HELO msg... and do nothing with it
       buffer = pkgClient->waitForMsg(PKGCPP_HELO);

       validate magic header that client should have sent
       if (strcmp(buffer, CLIENT_NAME) != 0)
       {
      Logger::getInstance()->logERROR(("Received a PKGCPP_HELO without proper data!\n");
       pkgClient->close();
       }

       //TODO probably should failsafe this loop also
       if (buffer == NULL)
       continue;
       */
    }
  while (pkgClient == NULL);

  int counter = 0;
  int itemsRemain = 0;
  /* we got a validated client, process packets from the
   * connection.  boilerplate triple-call loop.
   */
  Logger::getInstance()->logINFO("PkgCppTest", "Processing data from client\n");
  do
    {
      bu_log("Loop Pass #%d\n", counter);

      /* process packets potentially received in a processing callback */
      itemsRemain = tryProcess(pkgClient);

      /* suck in data from the network */
      int pkg_result = pkgClient->pullDataFromSocket();
      if (pkg_result < 0)
        {
    	  Logger::getInstance()->logINFO("PkgCppTest", "Seemed to have trouble sucking in packets.\n");
          break;
        }
      else if (pkg_result == 0)
        {
    	  Logger::getInstance()->logINFO("PkgCppTest", "Client closed the connection.\n");
          //usleep(100);
          //break;
        }

      bu_log("Last pass: %d bytes.\n", pkg_result);

      /* process new packets received */
      itemsRemain = tryProcess(pkgClient);

      counter++;
    }
  while (pkgClient != NULL && itemsRemain != 0 && listenLoop);

  Logger::getInstance()->logINFO("PkgCppTest", "Done with client.  Closing connection.\n");

  pkgClient->close();

  sleep(1);
  delete pkgClient;

  return 0;
}

int
runClient(std::string ipOrHostname, int port)
{
  //Generate random data block.
  char data[RAND_DATA_SIZE] = { 0 };

  for (int i = 0; i < RAND_DATA_SIZE; ++i)
    {
      data[i] = rand() % 0xFF;
    }

  //Create PkgClient obj and open new connection
  PkgTcpClient* connToServer = new PkgTcpClient(ipOrHostname, port, NULL);

  if (!connToServer->hasGoodConnection())
    {
      bu_log("Connection to '%s:%d' failed.\n", ipOrHostname.c_str(), port);
      bu_bomb("ERROR: Unable to open a connection to the server\n");
    }

  Logger::getInstance()->logINFO("PkgCppTest", "Sending HELO to server.\n");

  int bytes = 0;

  //Send a HELO
  bytes = connToServer->send(PKGCPP_HELO, CLIENT_NAME, strlen(CLIENT_NAME) + 1);
  if (bytes < 0)
    {
      connToServer->close();
      bu_log("Connection to %s, port %d, seems faulty.\n",
          ipOrHostname.c_str(), port);
      bu_bomb("ERROR: Unable to communicate with the server\n");
      return 1;
    }

  for (int i = 0; i < 2; ++i)
    {
      bu_log("Sending %d bytes to server.\n", RAND_DATA_SIZE);

      //Send Data
      bytes = connToServer->send(PKGCPP_DATA, data, RAND_DATA_SIZE);
      if (bytes < 0)
        {
          connToServer->close();
          bu_log("Connection to %s, port %d, seems faulty.\n",
              ipOrHostname.c_str(), port);
          bu_bomb("ERROR: Unable to send data to the server\n");
        }

      bu_log("Sent %d bytes to server.\n", bytes);

      sleep(1);
    }

  Logger::getInstance()->logINFO("PkgCppTest", "Sending CIAO.\n");
  /* let the server know we're done.  not necessary, but polite. */
  bytes = connToServer->send(PKGCPP_CIAO, "CIAO", 4);
  if (bytes < 0)
    {
      bu_log("Unable to cleanly disconnect from %s, port %d.\n",
          ipOrHostname.c_str(), port);
    }

  sleep(5);
  Logger::getInstance()->logINFO("PkgCppTest", "Disconnecting.\n");
  connToServer->close();
  delete connToServer;
  return 0;
}

int
tryProcess(PkgClient* pkgClient)
{
  int pkg_result = pkgClient->processData();
  if (pkg_result < 0)
    {
	  Logger::getInstance()->logINFO("PkgCppTest", "Unable to process packets? Weird.\n");
    }
  return pkg_result;
}

/**
 * callback when a HELO message packet is received.
 *
 * We should not encounter this packet specifically since we listened
 * for it before beginning processing of packets as part of a simple
 * handshake setup.
 */
void
server_helo(struct pkg_conn* c, char* buf)
{
  c = c; /* quell */
  int lenDataRecved = c->pkc_inend - sizeof(pkg_header);
  std::string name(buf);
  bu_log("HELO recv-ed from '%s':  %d bytes.\n", name.c_str(), lenDataRecved);
  free(buf);
}

/**
 * callback when a DATA message packet is received
 */
void
server_data(struct pkg_conn* c, char* buf)
{
  c = c; /* quell */
  int lenDataRecved = c->pkc_inend - sizeof(pkg_header);
  bu_log("DATA recv-ed:  %d bytes.\n", lenDataRecved);
  free(buf);
}

/**
 * callback when a CIAO message packet is received
 */
void
server_ciao(struct pkg_conn* c, char* buf)
{
  c = c; /* quell */
  int lenDataRecved = c->pkc_inend - sizeof(pkg_header);
  bu_log("CIAO recv-ed:  %d bytes.\n", lenDataRecved);
  free(buf);
  listenLoop = false;
}

/**
 * Prints the 'usage' statement to the console along with an optional message
 */
void
printUsage(std::string customMsg)
{
  if (customMsg.length() > 0)
    {
      std::cout << customMsg << std::endl;
    }

  std::cout << "Usage for Client: pkgcppTest client ipAddress port."
      << std::endl;
  std::cout << "Usage for Server: pkgcppTest server port." << std::endl;
  return;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
