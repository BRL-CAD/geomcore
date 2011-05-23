/*            N E T M S G S E R I A L T E S T . C X X
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
/** @file netMsgSerialTest.cxx
 *
 * Brief description
 *
 */

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

#include <string>

static int verbose = 0x3;

void logInfo(std::string s) {
	Logger::getInstance()->logINFO("NetMsgSerialTest", s);
}
void logBanner(std::string s) {
	Logger::getInstance()->logBANNER("NetMsgSerialTest", s);
}

void testMsg(NetMsg* msg01, std::string typeName) {
	NetMsgFactory* factory = NetMsgFactory::getInstance();

	ByteArray* networkSim = new ByteArray();
	msg01->serialize(networkSim);

	NetMsg* msg02 = factory->deserializeNetMsg(*networkSim, NULL);

	bool pass = (msg02 != NULL) && (*msg01 == *msg02);

	if (pass) {
		if(verbose&1)
			logInfo(typeName + ": \x1B[32mPASSED\x1B[m");
	} else {
		logInfo(typeName + ": \x1B[31mFAILED\x1B[m");
		if(verbose&2) {
			logInfo("\tMsg01: " + msg01->toString());
			if(msg02)
				logInfo("\tMsg02: " + msg02->toString());
			else
				logInfo("\tMsg02: (NULL)");
		}
	}
	delete msg02;
}

/*
 * =====================
 *
 *        Main
 *
 * =====================
 */

int main(int argc, char* argv[]) {
	//Setup common values to use
	std::string strUUID01("{60a03846-c39b-42e6-865f-394056a4fa04}");
	std::string strUUID02("{90645abd-3109-4538-a425-07810542cc2d}");
	std::string strUUID03("{732986e8-5ef9-4329-b457-bc83df959e1f}");
	std::string strUUID04("{84d05702-41c4-449d-947b-3c18a8f93cd9}");
	std::string strUUID05("{b2dd5d49-1654-49f4-83b2-512b9e2fc4dc}");
	std::string strUUID06("{ada2005b-02e1-4431-b7e8-432def490632}");

	std::list<std::string>* items = new std::list<std::string> ();
	items->push_back(strUUID01);
	items->push_back(strUUID02);
	items->push_back(strUUID03);
	items->push_back(strUUID04);
	items->push_back(strUUID05);
	items->push_back(strUUID06);

	if(argc == 3 && argv[1][0] == '-' && argv[1][1] == 'v')
		verbose = atoi(argv[2]);

	Logger::getInstance();

	/* Test Normal */
	TypeOnlyMsg msg001(DISCONNECTREQ);
	testMsg(&msg001, "TypeOnlyMsg-Normal");
	/* Test Reply */
	TypeOnlyMsg msg002(DISCONNECTREQ, &msg001);
	testMsg(&msg002, "TypeOnlyMsg-Reply");

	/* Test Normal */
	GenericFourBytesMsg msg011(TEST_GENERIC_4BYTE_MSG, 8675309);
	testMsg(&msg011, "GenericFourBytesMsg-Normal");
	/* Test Reply */
	GenericFourBytesMsg msg012(TEST_GENERIC_4BYTE_MSG, &msg011, 8675309);
	testMsg(&msg012, "GenericFourBytesMsg-Reply");

	/* Test Normal */
	GenericTwoBytesMsg msg031(TEST_GENERIC_2BYTE_MSG, 5309);
	testMsg(&msg031, "GenericTwoBytesMsg-Normal");
	/* Test Reply */
	GenericTwoBytesMsg msg032(TEST_GENERIC_2BYTE_MSG, &msg031, 5309);
	testMsg(&msg032, "GenericTwoBytesMsg-Reply");

	/* Test Normal */
	GenericOneByteMsg msg041(TEST_GENERIC_1BYTE_MSG, 42);
	testMsg(&msg041, "GenericOneByteMsg-Normal");
	/* Test Reply */
	GenericOneByteMsg msg042(TEST_GENERIC_1BYTE_MSG, &msg041, 42);
	testMsg(&msg042, "GenericOneByteMsg-Reply");

	std::string s05 = strUUID01;
	char* data05 = (char*) s05.c_str();
	ByteArray ba05(data05, s05.length());

	/* Test Normal */
	GenericMultiByteMsg
			msg051(TEST_GENERIC_MULTIBYTE_MSG, &ba05);
	testMsg(&msg051, "GenericMultiByteMsg-Normal");
	/* Test Reply */
	GenericMultiByteMsg msg052(TEST_GENERIC_MULTIBYTE_MSG, &msg051, &ba05);
	testMsg(&msg052, "GenericMultiByteMsg-Reply");

	/* Test Normal */
	GenericOneStringMsg msg061(TEST_GENERIC_1STRING_MSG, s05);
	testMsg(&msg061, "GenericOneStringMsg-Normal");
	/* Test Reply */
	GenericOneStringMsg msg062(TEST_GENERIC_1STRING_MSG, &msg061, s05);
	testMsg(&msg062, "GenericOneStringMsg-Reply");

	/* Test Normal */
	FailureMsg msg071(42);
	testMsg(&msg071, "FailureMsg-Normal");
	/* Test Reply */
	FailureMsg msg072(&msg071, 42);
	testMsg(&msg072, "FailureMsg-Reply");

	/* Test Normal */
	SuccessMsg msg081(42);
	testMsg(&msg081, "SuccessMsg-Normal");
	/* Test Reply */
	SuccessMsg msg082(&msg081, 42);
	testMsg(&msg082, "SuccessMsg-Reply");

	std::string s09 = strUUID02;
	char* data09 = (char*) s09.c_str();
	ByteArray ba09(data09, s09.length());

	/* Test Normal */
	GeometryChunkMsg msg091("path/to/file.g", &ba09);
	testMsg(&msg091, "GeometryChunkMsg-Normal");
	/* Test Reply */
	GeometryChunkMsg msg092(&msg091, "path/to/file.g", &ba09);
	testMsg(&msg092, "GeometryChunkMsg-Reply");

	/* Test Normal */
	GeometryManifestMsg msg101(*items);
	testMsg(&msg101, "GeometryManifestMsg-Normal");
	/* Test Reply */
	GeometryManifestMsg msg102(&msg101, *items);
	testMsg(&msg102, "GeometryManifestMsg-Reply");

	/* Test Normal */
	GeometryReqMsg msg111("/path/to/a/geometry/resource", false);
	testMsg(&msg111, "GeometryReqMsg-Normal");
	/* Test Reply */
	GeometryReqMsg msg112(&msg111, "/path/to/a/geometry/resource", false);
	testMsg(&msg112, "GeometryReqMsg-Reply");

	/* Test Normal */
	NewNodeOnNetMsg msg121("Kiaser Sose");
	testMsg(&msg121, "NewNodeOnNetMsg-Normal");
	/* Test Reply */
	NewNodeOnNetMsg msg122(&msg121, "Kiaser Sose");
	testMsg(&msg122, "NewNodeOnNetMsg-Reply");

	/* Test Normal */
	NewSessionReqMsg msg131("Kiaser Sose", "YourMom");
	testMsg(&msg131, "NewSessionReqMsg-Normal");
	/* Test Reply */
	NewSessionReqMsg msg132(&msg131, "Kiaser Sose", "YourMom");
	testMsg(&msg132, "NewSessionReqMsg-Reply");

	/* Test Normal */
	RemoteNodenameSetMsg msg141("RogerRamJet");
	testMsg(&msg141, "RemoteNodenameSetMsg-Normal");
	/* Test Reply */
	RemoteNodenameSetMsg msg142(&msg141, "RogerRamJet");
	testMsg(&msg142, "RemoteNodenameSetMsg-Reply");

	GSUuid* uuid = GSUuid::createUuid();

	/* Test Normal */
	SessionInfoMsg msg151(uuid);
	testMsg(&msg151, "SessionInfoMsg-Normal");
	/* Test Reply */
	SessionInfoMsg msg152(&msg151, uuid);
	testMsg(&msg152, "SessionInfoMsg-Reply");

	delete items;

	return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
