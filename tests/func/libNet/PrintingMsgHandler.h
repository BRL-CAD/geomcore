/*            P R I N T I N G M S G H A N D L E R . H
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
/** @file PrintingMsgHandler.h
 * PrintingMsgHandler.h
 *
 */

#ifndef __PRINTINGMSGHANDLER_H__
#define __PRINTINGMSGHANDLER_H__

#include "libnet.h"

#include <string>

class PrintingMsgHandler : public INetMsgHandler
{
public:
    PrintingMsgHandler(QString name): _name(name) {}

    void handleNetMsg(NetMsg* msg, NetPortal* origin)
    {
	Logger::getInstance()->logINFO(_name, "Received msg type '" + QString::number(msg->getMsgType()) + "' from '" + origin->getRemoteNodename() + "'");
    }

private:
    std::string _name;
};
#endif /* __PRINTINGMSGHANDLER_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
