/*                     P O N G M S G . C X X
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
/** @file PongMsg.cxx
 * PongMsg.cxx
 *
 *  Created on: Dec 20, 2010
 */

#include "NetMsgTypes.h"
#include "PongMsg.h"

PongMsg::PongMsg(PingMsg* ping): GenericEightBytesMsg(PONG, ping->getStartTime()){}
PongMsg::PongMsg(QDataStream* ds, Portal* origin): GenericEightBytesMsg(ds, origin) {}

PongMsg::~PongMsg(){}

uint64_t
PongMsg::getStartTime(){
	return this->data;
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
