/*         G E O M E T R Y S E R V I C E D A E M O N . H
 * BRLCAD
 *
 * Copyright (c) 2010 United States Government as represented by
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
/** @file GeometryServiceDaemon.h
 *
 * Brief description
 *
 */

#ifndef __GEOMETRYSERVICEDAEMON_H__
#define __GEOMETRYSERVICEDAEMON_H__

#include "GeometryServiceApp.h"

#include <QThread>

class GeometryServiceDaemon: public QThread
{
Q_OBJECT

public:
    GeometryServiceDaemon(GeometryServiceApp* gsa);
    virtual ~GeometryServiceDaemon(){};
    void stop();
    GeometryServiceApp* getGeometryServiceApp();

protected:
    void run();

private:
    GeometryServiceApp* gsa;
};

#endif

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
