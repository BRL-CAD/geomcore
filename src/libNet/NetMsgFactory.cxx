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
NetMsgFactory::deserializeNetMsg(QByteArray& data, Portal* origin)
{
  QDataStream temp(data);
  uint16_t msgType = 0;
  temp >> msgType;
  QDataStream qds(data);

  /* TODO Replace this with a map for registration scheme */
  switch (msgType)
    {
  case TEST_GENERIC_4BYTE_MSG:
    return new GenericFourBytesMsg(&qds, origin);

  case TEST_GENERIC_2BYTE_MSG:
    return new GenericTwoBytesMsg(&qds, origin);

  case TEST_GENERIC_1BYTE_MSG:
    return new GenericOneByteMsg(&qds, origin);

  case TEST_GENERIC_MULTIBYTE_MSG:
    return new GenericMultiByteMsg(&qds, origin);

  case TEST_GENERIC_1STRING_MSG:
    return new GenericOneStringMsg(&qds, origin);

  case RUALIVE:
    return new TypeOnlyMsg(&qds, origin);
  case IMALIVE:
    return new TypeOnlyMsg(&qds, origin);

  case FAILURE:
    return new GenericOneByteMsg(&qds, origin);
  case SUCCESS:
    return new GenericOneByteMsg(&qds, origin);
  case GS_REMOTE_NODENAME_SET:
    return new GenericOneStringMsg(&qds, origin);

  case DISCONNECTREQ:
    return new TypeOnlyMsg(&qds, origin);

  case NEWNODEONNET:
    return new GenericOneStringMsg(&qds, origin);
    /*     case FULL_NODE_LISTREQ: */
    /* 	return new NetMsg(&qds, origin); */
    /*     case FULL_NODE_LIST: */
    /* 	return new NetMsg(&qds, origin); */

  case NEWSESSIONREQ:
    return new NewSessionReqMsg(&qds, origin);
  case SESSIONINFO:
    return new SessionInfoMsg(&qds, origin);

  case GEOMETRYREQ:
    return new GeometryReqMsg(&qds, origin);
  case GEOMETRYMANIFEST:
    return new GeometryManifestMsg(&qds, origin);
  case GEOMETRYCHUNK:
    return new GeometryChunkMsg(&qds, origin);

  case PING:
    return new PingMsg(&qds, origin);
  case PONG:
    return new PongMsg(&qds, origin);


    /* Admin commands */
  case CMD_SHUTDOWN:
    return new TypeOnlyMsg(&qds, origin);


  default:
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
