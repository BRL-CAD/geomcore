/*                      N E T M S G F A C T O R Y. C X X
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
/** @file NetMsgFactory.cxx
 *
 * Brief description
 *
 */

#include <arpa/inet.h>	/* ntohs */

#include "NetMsgTypes.h"
#include "Portal.h"

#include "NetMsgFactory.h"
#include "GenericOneStringMsg.h"
#include "GenericOneByteMsg.h"
#include "GenericTwoBytesMsg.h"
#include "GenericFourBytesMsg.h"
#include "GenericMultiByteMsg.h"
#include "GeometryReqMsg.h"
#include "GeometryManifestMsg.h"
#include "GeometryChunkMsg.h"
#include "NewSessionReqMsg.h"
#include "SessionInfoMsg.h"
#include "TypeOnlyMsg.h"
#include "PingMsg.h"
#include "PongMsg.h"

NetMsgFactory* NetMsgFactory::pInstance = NULL;

NetMsgFactory*
NetMsgFactory::getInstance()
{
  if (NetMsgFactory::pInstance == NULL) {
    NetMsgFactory::pInstance = new NetMsgFactory();
  }
  return NetMsgFactory::pInstance;
}

NetMsgFactory::NetMsgFactory()
{}

NetMsgFactory::~NetMsgFactory()
{}

NetMsg*
NetMsgFactory::deserializeNetMsg(ByteBuffer* bb, Portal* origin)
{
  /* Check if at least header data is here. */
  if(bb->remaining() < 43) {
      return NULL;
  }

   NetMsg* msg = NULL;

  /* Peek at type */
  int start = bb->position();
  uint16_t msgType = bb->get16bit();
  bb->setPosition(start);

//  std::cout <<"Attempting to deserialize type: " << msgType << "\n";

  /* TODO Replace this with a map for registration scheme */
  switch (msgType)
    {
  case TEST_GENERIC_4BYTE_MSG:
    msg = new GenericFourBytesMsg(bb, origin);
    break;

  case TEST_GENERIC_2BYTE_MSG:
    msg = new GenericTwoBytesMsg(bb, origin);
    break;

  case TEST_GENERIC_1BYTE_MSG:
    msg = new GenericOneByteMsg(bb, origin);
    break;

  case TEST_GENERIC_MULTIBYTE_MSG:
    msg = new GenericMultiByteMsg(bb, origin);
    break;

  case TEST_GENERIC_1STRING_MSG:
    msg = new GenericOneStringMsg(bb, origin);
    break;

  case RUALIVE:
    msg = new TypeOnlyMsg(bb, origin);
    break;
  case IMALIVE:
    msg = new TypeOnlyMsg(bb, origin);
    break;

  case FAILURE:
    msg = new GenericOneByteMsg(bb, origin);
    break;
  case SUCCESS:
    msg = new GenericOneByteMsg(bb, origin);
    break;
  case GS_REMOTE_NODENAME_SET:
    msg = new GenericOneStringMsg(bb, origin);
    break;

  case DISCONNECTREQ:
    msg = new TypeOnlyMsg(bb, origin);
    break;

  case NEWNODEONNET:
    msg = new GenericOneStringMsg(bb, origin);
    break;
    /*     case FULL_NODE_LISTREQ: */
    /* 	msg = new NetMsg(bb, origin); */
    /*     case FULL_NODE_LIST: */
    /* 	msg = new NetMsg(bb, origin); */
    /*      break;     */

  case NEWSESSIONREQ:
    msg = new NewSessionReqMsg(bb, origin);
    break;
  case SESSIONINFO:
    msg = new SessionInfoMsg(bb, origin);
    break;

  case GEOMETRYREQ:
    msg = new GeometryReqMsg(bb, origin);
    break;
  case GEOMETRYMANIFEST:
    msg = new GeometryManifestMsg(bb, origin);
    break;
  case GEOMETRYCHUNK:
    msg = new GeometryChunkMsg(bb, origin);
    break;

  case PING:
    msg = new PingMsg(bb, origin);
    break;
  case PONG:
    msg = new PongMsg(bb, origin);
    break;


    /* Admin commands */
  case CMD_SHUTDOWN:
    msg = new TypeOnlyMsg(bb, origin);
    break;

  default:
    std::stringstream ss;
    ss << "Unknown Msgtype: ";
    ss << msgType;
    Logger::getInstance()->logERROR("NetMsgFactory", ss.str());

    return NULL;
    }


//  std::cout << "Finished deserializing.... \n";
  return msg;
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
