/*                  L I B N E T T E S T . C X X
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
/** @file libNetTest.cxx
 *
 * Brief description
 *
 */
#include <chrono>
#include <thread>

// https://stackoverflow.com/a/10613664/2037687
#define sleep(c) std::this_thread::sleep_for(std::chrono::seconds(c));

#include "bu.h"

#include "Logger.h"
#include "NetMsg.h"
#include "NetMsgFactory.h"
#include "TypeOnlyMsg.h"
#include "NetMsgTypes.h"
#include "GenericOneByteMsg.h"
#include "GenericTwoBytesMsg.h"
#include "GenericFourBytesMsg.h"
#include "GenericMultiByteMsg.h"
#include "GenericOneStringMsg.h"
#include "FailureMsg.h"
#include "SuccessMsg.h"
#include "GeometryChunkMsg.h"
#include "GeometryManifestMsg.h"
#include "GeometryReqMsg.h"
#include "NewNodeOnNetMsg.h"
#include "NewSessionReqMsg.h"
#include "RemoteNodenameSetMsg.h"
#include "SessionInfoMsg.h"
#include "JobManager.h"
#include "PortalManager.h"
#include "Portal.h"

#include <string>
#include <sstream>
#include <algorithm>

void logInfo(std::string s) {
	Logger::getInstance()->logINFO("libNetTest", s);
}
void logBanner(std::string s) {
	Logger::getInstance()->logBANNER("libNetTest", s);
}

/**
 * Prints the 'usage' statement to the console along with an optional message
 */
void printUsage(std::string customMsg) {
	if (customMsg.length() > 0) {
		customMsg += "\n";
		std::string s(customMsg.c_str());
		logInfo(s);
	}
	logInfo("Usage for Client: pkgcppTest client ipAddress port.\n");
	logInfo("Usage for Server: pkgcppTest server port.\n");

	return;
}

/**
 * Converts char* data to a valid port number
 */
int getValidPort(char* data) {
	std::string portStr(data);
	int port = atoi(data);

	/* Hardcode prolly not best for OS determined port range.... */
	if (port > 0x0000 && port < 0xFFFF) {
		/* More validation goes here, if needed. */
	} else {
		printUsage("Supplied Port '" + portStr + "' is invalid.");
		bu_exit(1, "");
	}
	return port;
}

/*
 * =====================
 *
 *        Main
 *
 * =====================
 */

int main(int argc, char* argv[]) {
	Logger::getInstance();
	JobManager::getInstance()->startup();

	logBanner("libNetTest");

	if (argc > 4 || argc < 3) {
		printUsage("Incorrect ARG count.");
		bu_exit(1, "");
	}

	bool isServer;
	short port;
	std::string ip("");

	/* Get app mode.  Either client or server */
	std::string cliServ(argv[1]);
	std::transform(cliServ.begin(), cliServ.end(), cliServ.begin(), tolower);

	if (cliServ == "client") {
		isServer = false;
		ip = argv[2];
		port = getValidPort(argv[3]);
	} else if (cliServ == "server") {
		isServer = true;
		port = getValidPort(argv[2]);
	} else {
		printUsage("Unknown mode: '" + cliServ + "'");
		bu_exit(1, "");
	}

	std::string s("Running in ");
	s.append(cliServ.c_str());
	s.append(" mode.");
	logInfo(s);

	if (isServer) {
		PortalManager pm("TestServer", port);
		pm.start();

		/* listen for a loooong time. */
		sleep(60 * 60); /* 1 hr */
		logInfo("Shutting down...");
		pm.shutdown();

	} else {
		std::ostringstream num;
		PortalManager pm("TestClient");
		pm.start();

		sleep(2);

		s = "Trying to connect to ";
		s.append(ip.c_str());
		s.append(":");
		num << port;
		s.append(num.str());
		logInfo(s);

		Portal* p = pm.connectToHost(ip, port);

		if (p != 0) {
			sleep(3);
			TypeOnlyMsg tom(RUALIVE);
			p->send(&tom);
			sleep(2);
			logInfo("Disconnecting...");
			p->disconnect();
		}

		sleep(2);
		logInfo("Shutting down Portal Manager...");
		pm.shutdown();
	}

	sleep(1);
	logInfo("Shutting down JobManager...");
	JobManager::getInstance()->shutdown(true);
	sleep(1);
	return 0;
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
