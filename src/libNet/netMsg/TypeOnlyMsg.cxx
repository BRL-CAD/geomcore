/*                 T Y P E O N L Y M S G . C X X
 * BRLCAD
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
/** @file TypeOnlyMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "TypeOnlyMsg.h"
#include <sstream>

/* Normal Constructor */
TypeOnlyMsg::TypeOnlyMsg(uint32_t msgType) :
    NetMsg(msgType)
{}

/* Reply Constructor */
TypeOnlyMsg::TypeOnlyMsg(uint32_t msgType, NetMsg* msg) :
	NetMsg(msgType, msg)
{}

/* Deserializing Constructor */
TypeOnlyMsg::TypeOnlyMsg(DataStream* ds, Portal* origin) :
     NetMsg(ds, origin)
{}

/* Destructor */
TypeOnlyMsg::~TypeOnlyMsg()
{}

bool TypeOnlyMsg::_serialize(DataStream* ds)
{
    return true;
}
bool TypeOnlyMsg::_equals(const NetMsg& msg)
{
    return true;
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
