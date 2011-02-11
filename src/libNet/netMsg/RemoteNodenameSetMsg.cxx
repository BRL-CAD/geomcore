/*        R E M O T E N O D E N A M E S E T M S G . C X X
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
/** @file RemoteNodenameSetMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "RemoteNodenameSetMsg.h"
#include <sstream>

/* Normal Constructor */
RemoteNodenameSetMsg::RemoteNodenameSetMsg(std::string localNodename) :
    GenericOneStringMsg(GS_REMOTE_NODENAME_SET, localNodename)
{}

/* Reply Constructor */
RemoteNodenameSetMsg::RemoteNodenameSetMsg(NetMsg* msg, std::string localNodename) :
	GenericOneStringMsg(GS_REMOTE_NODENAME_SET, msg, localNodename)
{}

/* Deserializing Constructor */
RemoteNodenameSetMsg::RemoteNodenameSetMsg(DataStream* ds, Portal* origin) :
    GenericOneStringMsg(ds, origin)
{}

/* Destructor */
RemoteNodenameSetMsg::~RemoteNodenameSetMsg()
{}

/*
 *Getters n Setters
 */
std::string RemoteNodenameSetMsg::getRemoteNodename()
{
    return this->strData;
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
