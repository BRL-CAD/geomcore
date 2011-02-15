/*               A C C O U N T M A N A G E R . C X X
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
/** @file AccountManager.h
 *
 * Interface to the SVN user system.
 *
 */

#ifndef __ACCOUNTMANAGER_H__
#define __ACCOUNTMANAGER_H__

#include "Logger.h"
#include "Account.h"
#include "Session.h"
#include "Portal.h"

#include <string>
#include <list>

#include <GSThread.h>

class AccountManager
{
public:
    virtual ~AccountManager();
    static AccountManager* getInstance();
    Account* login(std::string uname, std::string passwd, Portal* p);
    void logout(Account* a);

private:
    static AccountManager* pInstance;
    AccountManager();

    Logger* log;
    GSMutex accountListLock;
    std::list<Account*>* accounts;

    int32_t validateLoginCreds(std::string uname, std::string passwd);
    Account* newAccount(std::string uname, Portal* p, uint32_t id);
    void remAccount(Account* a);

	/* Disable copy cstr and =operator */
    AccountManager(AccountManager const&){};
    AccountManager& operator=(AccountManager const&){};
};

#endif /* __ACCOUNTMANAGER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
