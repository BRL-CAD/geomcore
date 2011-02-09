/*         N E T P O R T A L M A N A G E R T E S T E R . H
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
/** @file NetPortalManagerTester.h
 *
 * Brief description
 *
 */

#ifndef __NETPORTALMANAGERTESTER_H__
#define __NETPORTALMANAGERTESTER_H__

#include "libnetwork.h"
#include "libutility.h"

#include <QObject>

class NetPortalManagerTester : public QObject
{
Q_OBJECT

public:
    NetPortalManagerTester(NetPortalManager* portMan);
    NetPortal* getNewPortal();

protected slots:
    void handleMsgReady();
    void handleHandshakeStatusUpdate(uint32_t current, uint32_t old);
    void handleHandshakeComplete(NetPortal* portal);

    void handlePortalConnected();
    void handlePortalDisonnected();
    void handleSocketError(QAbstractSocket::SocketError err);

private:
    Logger* log;
    NetPortalManager* portMan;
};

#endif

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8

