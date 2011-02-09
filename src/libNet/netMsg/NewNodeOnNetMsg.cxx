/*             N E W N O D E O N N E T M S G . C X X
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
/** @file NewNodeOnNetMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "NewNodeOnNetMsg.h"
#include <sstream>

/* Normal Constructor */
NewNodeOnNetMsg::NewNodeOnNetMsg(std::string nodename) :
    GenericOneStringMsg(GS_REMOTE_NODENAME_SET, nodename)
{}

/* Reply Constructor */
NewNodeOnNetMsg::NewNodeOnNetMsg(NetMsg* msg, std::string nodename) :
    GenericOneStringMsg(GS_REMOTE_NODENAME_SET, msg, nodename)
{}

/* Deserializing Constructor */
NewNodeOnNetMsg::NewNodeOnNetMsg(QDataStream* ds, Portal* origin) :
    GenericOneStringMsg(ds, origin)
{}

/* Destructor */
NewNodeOnNetMsg::~NewNodeOnNetMsg()
{}

/*
 *Getters n Setters
 */
std::string NewNodeOnNetMsg::getNewNodename()
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
