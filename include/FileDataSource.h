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

class FileDataSource : public IDataSource
{
public:
	FileDataSource(std::string repoPath);
	virtual ~FileDataSource();

	/* Get a single DbObject */
	DbObject* getObj(std::string path);

	/* Get a single Attribute of an object */
	prop* getAttr(std::string path, std::string attrKey);

	/* Get a set of objects */
	std::list<DbObject*>* getObjs(std::string path);

	/* Get all Attributes from object */
	std::list<prop>* getAttrs(std::string path);

	bool init();

private:
	std::string repoPath;

	std::list<std::string> locks;
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
