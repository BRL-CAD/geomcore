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
  /* Check if bb ready for read */
  if(bb->position() != 0) {
      bb->flip();
  }

  /* Check if at least header data is here. */
  if(bb->limit() < 43) {
      return NULL;
  }

  /* Peek at type */
  int start = bb->position();
  uint16_t msgType = bb->get16bit();
  bb->setPosition(start);

  /* TODO Replace this with a map for registration scheme */
  switch (msgType)
    {
  case TEST_GENERIC_4BYTE_MSG:
    return new GenericFourBytesMsg(bb, origin);

  case TEST_GENERIC_2BYTE_MSG:
    return new GenericTwoBytesMsg(bb, origin);

  case TEST_GENERIC_1BYTE_MSG:
    return new GenericOneByteMsg(bb, origin);

  case TEST_GENERIC_MULTIBYTE_MSG:
    return new GenericMultiByteMsg(bb, origin);

  case TEST_GENERIC_1STRING_MSG:
    return new GenericOneStringMsg(bb, origin);

  case RUALIVE:
    return new TypeOnlyMsg(bb, origin);
  case IMALIVE:
    return new TypeOnlyMsg(bb, origin);

  case FAILURE:
    return new GenericOneByteMsg(bb, origin);
  case SUCCESS:
    return new GenericOneByteMsg(bb, origin);
  case GS_REMOTE_NODENAME_SET:
    return new GenericOneStringMsg(bb, origin);

  case DISCONNECTREQ:
    return new TypeOnlyMsg(bb, origin);

  case NEWNODEONNET:
    return new GenericOneStringMsg(bb, origin);
    /*     case FULL_NODE_LISTREQ: */
    /* 	return new NetMsg(bb, origin); */
    /*     case FULL_NODE_LIST: */
    /* 	return new NetMsg(bb, origin); */

  case NEWSESSIONREQ:
    return new NewSessionReqMsg(bb, origin);
  case SESSIONINFO:
    return new SessionInfoMsg(bb, origin);

  case GEOMETRYREQ:
    return new GeometryReqMsg(bb, origin);
  case GEOMETRYMANIFEST:
    return new GeometryManifestMsg(bb, origin);
  case GEOMETRYCHUNK:
    return new GeometryChunkMsg(bb, origin);

  case PING:
    return new PingMsg(bb, origin);
  case PONG:
    return new PongMsg(bb, origin);


    /* Admin commands */
  case CMD_SHUTDOWN:
    return new TypeOnlyMsg(bb, origin);


  default:
    std::stringstream ss;
    ss << "Unknown Msgtype: ";
    ss << msgType;
    Logger::getInstance()->logERROR("NetMsgFactory", ss.str());

    return NULL;
    }
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
