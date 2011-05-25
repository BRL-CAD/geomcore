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
#include "StringUtils.h"
#include "BrlcadDb.h"

#include "db.h"
#include "raytrace.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <algorithm>
#include <dirent.h>
#include <iostream>


FileDataSource::FileDataSource(std::string repoPath)
{
  this->repoPath = repoPath;
  StringUtils::cleanString(&this->repoPath);
}

FileDataSource::~FileDataSource()
{}

/* get a directory listing or a child list */
int
FileDataSource::getListing(std::string path, std::list<std::string>* list)
{
  std::string absPath = "";
  StringUtils::combinePaths(&absPath, &this->repoPath, &path);

  int pathStep = 0;

  std::string fsPath;
  std::string gPath;
  int totalSteps = 0;
  int splitPoint = StringUtils::splitPathAtFile(absPath, &fsPath, &gPath, &totalSteps);

  if (splitPoint == totalSteps) {
      /* We ended the path on a FS Dir or File */
      int type = StringUtils::isFileOrDir(fsPath.c_str());
      if (type <= 0)
          return -1; /* 0 == NOT EXIST */
      if (type == 1)
          return FileDataSource::getFsDirList(fsPath, list); /* 1 == DIR */
      /* Allow type == 2 (aka G File) to fall through to G processing code. */
  }

  return FileDataSource::getGChildList(fsPath, gPath, list);
}

/* Get a set of BRLCAD::MinimalObjects */
std::list<BRLCAD::MinimalObject*>*
FileDataSource::getObjs(std::string relPath, bool recurse)
{
	std::string absPath = "";

	StringUtils::combinePaths(&absPath, &this->repoPath, &relPath);

	//figure out what kind of path we are dealing with;
	if (StringUtils::isFileOrDir(absPath.c_str()) == 0)
		return NULL;

	if (StringUtils::isFileOrDir(absPath.c_str()) == 1)
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
  Logger::getInstance()->logINFO("FileDataSource",
      this->repoPath + " is readable.");

  if (bu_file_writable(path) <= 0)
    return false;
  Logger::getInstance()->logINFO("FileDataSource",
      this->repoPath + " is writable.");

  return true;
}

int
FileDataSource::getFsDirList(std::string dir, std::list<std::string>* files)
{
  DIR *dp;
  struct dirent *dirp;
  if ((dp = opendir(dir.c_str())) == NULL)
    return errno;

  while ((dirp = readdir(dp)) != NULL)
    files->push_back(std::string(dirp->d_name));

  closedir(dp);
  return 0;
}

int
FileDataSource::getGChildList(
    std::string fsPath,
    std::string gPath,
    std::list<std::string>* items)
{
  //TODO start caching these BrlcadDB objects!!!!
  BrlcadDb* db = BrlcadDb::makeDb(fsPath);
  if (db == NULL) return BrlcadDb::FS_PATH_NOT_VALID;

  std::string name = StringUtils::getLastStepOfPath(gPath);

  int retVal = db->list(name, items);
  delete db;
  return retVal;
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
