/*                       P I N G M S G . H
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
/** @file PingMsg.h
 * PingMsg.h
 *
 *  Created on: Dec 20, 2010
 */

#ifndef __PINGMSG_H__
#define __PINGMSG_H__

#include "GenericEightBytesMsg.h"

class PingMsg: public GenericEightBytesMsg {
public:
	/* Normal Constructor */
	PingMsg(uint64_t startT);

	/* Reply Constructor */
	PingMsg(NetMsg* msg, uint64_t startT);

	/* Deserializing Constructor */
	PingMsg(ByteBuffer* bb, Portal* origin);

	/* Destructor */
	virtual ~PingMsg();

	uint64_t getStartTime();
};

#endif /* __PINGMSG_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
