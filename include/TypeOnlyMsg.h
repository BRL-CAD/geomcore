/*                 T Y P E O N L Y M S G . C X X
 * BRL-CAD
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
/** @file TypeOnlyMsg.cxx
 *
 * Brief description
 *
 */

#ifndef __TYPEONLYMSG_H__
#define __TYPEONLYMSG_H__

#include "NetMsg.h"

class TypeOnlyMsg: public NetMsg
{

public:
    /* Normal Constructor */
    TypeOnlyMsg(quint32 msgType);

    /* Reply Constructor */
    TypeOnlyMsg(quint32 msgType, NetMsg* msg);

    /* Deserializing Constructors */
    TypeOnlyMsg(QDataStream* ds, QString origin);

    /* Destructor */
    virtual ~TypeOnlyMsg();

protected:
    bool _serialize(QDataStream* ds);
    bool _equals(NetMsg& msg);
};

#endif /* __TYPEONLYMSG_H__ */

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
