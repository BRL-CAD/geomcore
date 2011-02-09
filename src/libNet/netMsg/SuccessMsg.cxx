/*                 S U C C E S S M S G . C X X
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
/** @file SuccessMsg.cxx
 *
 * Brief description
 *
 */

#include "NetMsgTypes.h"
#include "SuccessMsg.h"
#include <sstream>

/* Normal Constructor */
SuccessMsg::SuccessMsg(uint8_t successCode) :
    GenericOneByteMsg(SUCCESS, successCode)
{}

/* Reply Constructor */
SuccessMsg::SuccessMsg(NetMsg* msg, uint8_t successCode) :
	GenericOneByteMsg(SUCCESS, msg, successCode)
{}

/* Deserializing Constructor */
SuccessMsg::SuccessMsg(QDataStream* ds, Portal* origin) :
    GenericOneByteMsg(ds, origin)
{}

/* Destructor */
SuccessMsg::~SuccessMsg()
{}

/*
 *Getters n Setters
 */
uint8_t SuccessMsg::getSuccessCode()
{
    return this->data;
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
