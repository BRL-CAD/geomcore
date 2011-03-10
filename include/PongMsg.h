/*                       P O N G M S G . H
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
/** @file PongMsg.h
 * PongMsg.h
 *
 *  Created on: Dec 20, 2010
 */

#ifndef __PONGMSG_H__
#define __PONGMSG_H__

#include "GenericEightBytesMsg.h"
#include "PingMsg.h"

#include <string>

class PongMsg: public GenericEightBytesMsg {
public:
	/* Normal & Reply Constructor */
	PongMsg(PingMsg* ping);

	/* Deserializing Constructor */
	PongMsg(DataStream* ds, Portal* origin);

	/* Destructor */
	virtual ~PongMsg();

	uint64_t getStartTime();
};

#endif /* __PONGMSG_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
