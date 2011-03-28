/*             A C C O U N T . C X X
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
/** @file Account.cxx
 *
 * Class that represents a user's account information.
 *
 */

#include "Account.h"

Account::Account(std::string uname, Portal* portal, uint32_t id) :
    uname(uname), portal(portal), id(id) {
    this->stampLastAccess();
}

Account::~Account()
{}

std::string
Account::getUname()
{
    return this->uname;
}

Portal*
Account::getPortal()
{
    return this->portal;
}

uint32_t
Account::getID()
{
    return this->id;
}

void
Account::stampLastAccess()
{
    this->lastAccess = time (NULL);
}

time_t
Account::getInactivityTime()
{
    time_t now = time(NULL);

    return now - this->lastAccess;
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
