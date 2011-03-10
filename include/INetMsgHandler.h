/*              I N E T M S G H A N D L E R . H
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
/** @file INetMsgHandler.h
 *
 * Interface.  Any class needing to handle incoming
 * NetMsgs and be registered to the NetMsgSubscriberRegistry
 * must implement this interface.
 *
 */

#ifndef __INETMSGHANDLER_H__
#define __INETMSGHANDLER_H__

#include "NetMsg.h"

class INetMsgHandler {
public:
	/**
	 * Handles a NetMsg.  Return value indicates whether the NetMsg has been fully handled and the memory released.
	 */
	virtual bool handleNetMsg(NetMsg* msg) = 0;
};

#endif /* __INETMSGHANDLER_H__ */

/*
 * Local Variables: ***
 * mode: C
 * tab-width: 8
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t
 * End: ***
 * ex: shiftwidth=4 tabstop=8
*/
