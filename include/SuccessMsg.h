/*             S U C C E S S M S G . H
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
/** @file SuccessMsg.h
 *
 * Brief description
 *
 */

#ifndef __SUCCESSMSG_H__
#define __SUCCESSMSG_H__

#include "GenericOneByteMsg.h"

class SuccessMsg : public GenericOneByteMsg
{
public:
	/* Normal Constructor */
	SuccessMsg(uint8_t successCode);

	/* Reply Constructor */
	SuccessMsg(NetMsg* msg, uint8_t successCode);

	/* Deserializing Constructor */
	SuccessMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~SuccessMsg();

	/*
	 *Getters n Setters
	 */
	uint8_t getSuccessCode();

private:
	/* Disable copy cstr and =operator */
	SuccessMsg(SuccessMsg const&):GenericOneByteMsg(0, (uint8_t)0){};
	SuccessMsg& operator=(SuccessMsg const&){};
};

#endif /* __SUCCESSMSG_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
