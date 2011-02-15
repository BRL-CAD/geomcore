/*                F I L E D A T A S O U R C E . H
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
/** @file FileDataSource.h
 *
 * Brief description
 *
 */

#ifndef __FILEDATASOURCE_H__
#define __FILEDATASOURCE_H__

#include "IDataSource.h"

#include <string>
#include <list>

#include <QtCore/QMutex>

class FileDataSource : public IDataSource
{
public:
	FileDataSource(std::string repoPath);
	virtual ~FileDataSource();

	bool lock(DbObject* obj, Account* a);
	bool hasLock(DbObject* obj, Account* a);
	bool unlock(DbObject* obj);

	DbObject* getByPath(std::string path);
	DbObject* getByID(GSUuid* id);
	bool putObject(DbObject* obj);

private:
	std::string repoPath;

	QMutex lockLock;
	std::list<std::string> pathLocks;
	bool hasPathLock(std::string path);
	void setPathLock(std::string path);
	void remPathLock(std::string path);

	/* Disable copy cstr and =operator */
	FileDataSource(FileDataSource const&){};
	FileDataSource& operator=(FileDataSource const&){};
};

#endif /* __FILEDATASOURCE_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
