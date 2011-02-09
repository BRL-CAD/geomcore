/*                     A C C O U N T . C X X
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
/** @file Account.h
 *
 * Class that represents a user's account information.
 *
 */

#ifndef __ACCOUNT_H__
#define __ACCOUNT_H__

#include "Portal.h"

#include <ctime>

#include <string>

class Account
{
public:
  Account(std::string uname, Portal* portal, uint32_t id);
  virtual ~Account();
  std::string getUname();
  time_t getInactivityTime();
  void stampLastAccess();
  uint32_t getID();
  Portal* getPortal();
  
private:
  uint32_t id;
  std::string uname;
  Portal* portal;

  time_t lastAccess;

private:
	/* Disable copy cstr and =operator */
	Account(Account const&){};
	Account& operator=(Account const&){};
};

#endif /* __ACCOUNT_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
