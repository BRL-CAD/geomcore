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
#include <MinimalObject.h>
#include <MinimalDatabase.h>
#include <string>
#include <list>

#define PATH_DELIM "/"

class FileDataSource : public IDataSource
{
public:
	FileDataSource(std::string repoPath);
	virtual ~FileDataSource();
	bool init();

	/*
	 * 'GET' ers
	 */

	/* get a directory listing or a child list */
	std::list<std::string>* getListing(std::string path);

	/* Get a set of objects */
	std::list<BRLCAD::MinimalObject*>* getObjs(std::string path, bool recurse);

	/*
	 * 'PUT' ers
	 */

	/* Put a single BRLCAD::Object */
	bool putObj(std::string path, BRLCAD::MinimalObject* obj);

	//TODO swap out with bu_file_type()
	/**
	 * Checks the supplied path for existence and whether its a file or directory.
	 * return values:
	 * 0 == does not exist
	 * 1 == exists and is a Directory
	 * 2 == exists and is a File
	 */
	static int existsFileOrDir(const char* path);

        static void buildFullPath(std::string* out, std::string* base, std::string* path);
        static void cleanString(std::string* out);

        static int walkPath(std::string path);
	static int pathToStringStack(std::string path, std::list<std::string>* stringStack);
private:
	std::string repoPath;
	std::list<std::string> locks;

        static int walkPathFS(const std::list<std::string>* strStack, const unsigned int stackPos);
        static int walkPathG(const std::list<std::string>* strStack, const unsigned int stackPos);
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
