/*            G E O M E T R Y C H U N K M S G . C X X
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
/** @file GeometryChunkMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "GeometryChunkMsg.h"
#include <sstream>

/* Normal Constructor */
GeometryChunkMsg::GeometryChunkMsg(char* dataIn, uint32_t dataInLen) :
    GenericMultiByteMsg(GEOMETRYCHUNK, dataIn, dataInLen)
{}

/* Reply Constructor */
GeometryChunkMsg::GeometryChunkMsg(NetMsg* msg, char* dataIn, uint32_t dataInLen) :
	GenericMultiByteMsg(GEOMETRYCHUNK, msg, dataIn, dataInLen)
{}

/* Deserializing Constructor */
GeometryChunkMsg::GeometryChunkMsg(QDataStream* ds, Portal* origin) :
    GenericMultiByteMsg(ds, origin)
{}

/* Destructor */
GeometryChunkMsg::~GeometryChunkMsg()
{}

/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
