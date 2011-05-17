/*              F I L E D A T A S O U R C E . C X X
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
/** @file FileDataSource.cxx
 *
 */

#include "FileDataSource.h"
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <algorithm>

FileDataSource::FileDataSource(std::string repoPath)
{
  this->repoPath = repoPath;
  FileDataSource::cleanString(&this->repoPath);
}

FileDataSource::~FileDataSource()
{}

/* get a directory listing or a child list */
std::list<std::string>*
FileDataSource::getListing(std::string path)
{
  std::string absPath = "";
  FileDataSource::buildFullPath(&absPath, &this->repoPath, &path);



}

/* Get a set of BRLCAD::MinimalObjects */
std::list<BRLCAD::MinimalObject*>*
FileDataSource::getObjs(std::string relPath, bool recurse)
{
	std::string absPath = "";

	FileDataSource::buildFullPath(&absPath, &this->repoPath, &relPath);


	//figure out what kind of path we are dealing with;
	if (this->existsFileOrDir(absPath.c_str()) == 0)
		return NULL;

	if (this->existsFileOrDir(absPath.c_str()) == 1)
		return NULL;

	std::list<BRLCAD::MinimalObject*>* out = NULL;

	BRLCAD::MinimalDatabase md;
	bool loaded = md.Load(absPath);

	if (!loaded) {
            perror(strerror(errno));
            Logger::getInstance()->logINFO("FileDataSource", "Failed to load.");
	    return out;
	}
        Logger::getInstance()->logINFO("FileDataSource", "File seemingly loaded.");

/*
	if (recurse)
	  out = md->getAllObjects();
	else
	  out = md->getAllTopObjects();
*/

	out = md.getAllObjs();

	return out;
}

/* Put a single BRLCAD::MinimalObject */
bool
FileDataSource::putObj(std::string path, BRLCAD::MinimalObject* obj)
{
    return true;
}

bool
FileDataSource::init()
{
	const char* path = this->repoPath.c_str();

	/* check to see if there is a repo at the supplied path, and if we can R/W to it. */
    if (bu_file_readable(path) <= 0)
    	return false;
    Logger::getInstance()->logINFO("FileDataSource", this->repoPath + " is readable.");

    if (bu_file_writable(path) <= 0)
    	return false;
    Logger::getInstance()->logINFO("FileDataSource", this->repoPath + " is writable.");

    return true;
}

int
FileDataSource::existsFileOrDir(const char* path)
{
    struct stat st_buf;

    /* Check existence */
    if ((stat (path, &st_buf)) != 0)
        return 0;

    /* Check if dir */
    if (S_ISDIR (st_buf.st_mode))
    	return 1;

    /* Check if file */
    if (S_ISREG (st_buf.st_mode))
    	return 2;

    /* Shouldn't ever get here. */
    return -1;
}

void
FileDataSource::buildFullPath(std::string* out, std::string* base, std::string* path)
{
  *out = *base + "/" + *path;
  FileDataSource::cleanString(out);
}

void
FileDataSource::cleanString(std::string* out)
{
  int pos = out->find("//");
  while (pos != -1)
    {
      out->replace(pos,2,"/");
      pos = out->find("//");
    }
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
