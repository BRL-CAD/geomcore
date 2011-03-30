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
/** @file AccountManager.cxx
 *
 * Interface to the SVN user system.
 *
 */

#include "AccountManager.h"
#include "SessionManager.h"

#include <cstdlib>


AccountManager* AccountManager::pInstance = NULL;


AccountManager::AccountManager()
{
    this->accounts = new std::list<Account*>();
    this->log = Logger::getInstance();
}


AccountManager::~AccountManager()
{
    delete this->accounts;
}


AccountManager*
AccountManager::getInstance()
{
    if (!AccountManager::pInstance) {
	pInstance = new AccountManager();
    }
    return AccountManager::pInstance;
}


/**
 * returns 0 for bad login.  Positive number is the accountID
 */
int32_t
AccountManager::validateLoginCreds(std::string uname, std::string passwd)
{
    /* TODO put in REAL account validation here. */
    if (uname == "Guest" && passwd == "Guest") {
	return 0;
    }
    if (uname == "Keyser" && passwd == "Soze") {
	return 1;
    }
    if (uname == "Dean" && passwd == "Keaton") {
	return 2;
    }
    if (uname == "Michael" && passwd == "McManus") {
	return 3;
    }
    if (uname == "Fred" && passwd == "Fenster") {
	return 4;
    }
    if (uname == "Todd" && passwd == "Hockney") {
	return 5;
    }
    if (uname == "Roger" && passwd == "Kint") {
	return 6;
    }

    return -1;
}


Account*
AccountManager::login(std::string uname, std::string passwd, Portal* p)
{
    int32_t id = 0;
    char buf[BUFSIZ];

    id = this->validateLoginCreds(uname, passwd);

    if (id < 0) {
	snprintf(buf, BUFSIZ, "Authentication FAILED. User: '%s', accountID: %d", uname.c_str(), id);
	log->logINFO("AccountManager", buf);
	return NULL;
    }

    snprintf(buf, BUFSIZ, "Authenticated user: '%s', accountID: %d", uname.c_str(), id);
    log->logINFO("AccountManager", buf);

    Account* acc = this->newAccount(uname, p, id);
    return acc;
}


void
AccountManager::logout(Account* a)
{
    this->remAccount(a);
}


Account*
AccountManager::newAccount(std::string uname, Portal* p, uint32_t id)
{
    Account* a = NULL;

    //check to see if its already cached.
    //New
    a = new Account(uname, p, id);
    
    //cache
    this->accountListLock.lock();
    this->accounts->push_back(a);
    this->accountListLock.unlock();

    return a;
}


void
AccountManager::remAccount(Account* a)
{
    this->accountListLock.lock();
    this->accounts->remove(a); /* TODO Removes matches to mem address only, upgrade this logic. */
    this->accountListLock.unlock();
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
