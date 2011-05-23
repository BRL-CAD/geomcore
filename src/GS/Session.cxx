/*                     S E S S I O N . C X X
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
/** @file Session.cxx
 *
 */

#include "Session.h"

Session::Session(Account* a)
{
    this->sessionID = GSUuid::createUuid();
    this->a = a;
    this->stampLastAccess();
}

Session::~Session()
{}

GSUuid* Session::getSessionID()
{
    return this->sessionID;
}

Account*  Session::getAccount()
{
    return this->a;
}

void
Session::stampLastAccess()
{
    this->lastAccess = time (NULL);
}

time_t
Session::getInactivityTime()
{
    time_t now = time(NULL);

    return now - this->lastAccess;
}

SessionInfoMsg*
Session::generateSessionInfoMsg()
{
    return new SessionInfoMsg(this->sessionID);
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
