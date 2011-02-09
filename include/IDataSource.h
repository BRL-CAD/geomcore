/*                  I D A T A S O U R C E . H
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
/** @file IDataSource.h
 *
 * Interface.  Defines the required methods needed
 * to be a DataSource for the DataManager.
 *
 */

#ifndef __IDATASOURCE_H__
#define __IDATASOURCE_H__

#include "DbObject.h"
#include "Account.h"

class IDataSource {
public:
	virtual bool lock(DbObject* obj, Account* a) = 0;
	virtual bool hasLock(DbObject* obj, Account* a) = 0;
	virtual bool unlock(DbObject* obj) = 0;

	virtual DbObject* getByPath(std::string path) = 0;
	virtual DbObject* getByID(QUuid id) = 0;
	virtual bool putObject(DbObject* obj) = 0;
};

#endif /* __IDATASOURCE_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
