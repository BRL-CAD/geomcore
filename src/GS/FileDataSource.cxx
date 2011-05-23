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
#include "db.h"
#include "raytrace.h"
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

int
FileDataSource::walkPath(std::string path)
{
  std::list<std::string> pathArr;
  FileDataSource::pathToStringStack(path, &pathArr);

  return FileDataSource::walkPathFS(&pathArr, 0);
}

int
FileDataSource::walkPathFS(const std::list<std::string>* strStack, const unsigned int stackPos)
{
  std::string pathSoFar = "";
  std::string pathStep = "";
  int step = stackPos;
  int ford = 0;
  std::list<std::string>::const_iterator it = strStack->begin();

  /* fast forward */
  for (int i = 0; i < stackPos; ++i)
    {
      pathStep = (std::string) *it;
      pathSoFar += pathStep;
      ++it;
    }

  for (; it != strStack->end(); ++it)
    {
      pathStep = (std::string)*it;
      pathSoFar += pathStep;
      ford = FileDataSource::existsFileOrDir(pathSoFar.c_str());

//      std::cout << "step: "<<pathStep<<" cumulative: "<<pathSoFar << " ford:" << ford << std::endl;

      if (ford == 2) {
          ++step;
          /* Goto walkPathG */
          return walkPathG(strStack, step);
      } else if (ford == 1) {
          ++step;
          pathSoFar += "/";
          continue;
      } else if (ford == 0) {
          return step * -1; /* Failed */
      } else {
          return step * -1; /* Failed */
      }
    }
}

int
FileDataSource::walkPathG(const std::list<std::string>* strStack, const unsigned int stackPos)
{
  std::string fsPath = "";
  std::string gPath = "";
  std::string pathStep = "";
  std::list<std::string>::const_iterator it = strStack->begin();
  struct db_i *dbip;
  struct directory *dp;
  struct db_full_path dfp;
  int dbStep = 0;
  int exists = 0;

  /* fast forward */
  for (int i = 0; i < stackPos; ++i)
    {
      pathStep = (std::string) *it;
      fsPath += pathStep;
      ++it;

      if (i != (stackPos-1))
        fsPath += "/";
    }

  /* Open DB file */
  if ((dbip = db_open(fsPath.c_str(), "r")) == DBI_NULL) {
      perror(fsPath.c_str());
      bu_exit(1, "Unable to open geometry file (%s)\n", fsPath.c_str());
  }
  if (db_dirbuild(dbip)) {
      bu_exit(1, "ERROR: db_dirbuild failed\n");
  }

  /* Assuming we are at TOPs here. */
  for (; it != strStack->end(); ++it)
    {
      pathStep = (std::string) *it;
      gPath += pathStep;

//      std::cout << "G: fsPath: " << fsPath
//          << " gPath:" << gPath << std::endl;

      db_full_path_init(&dfp);
      exists = db_string_to_path(&dfp, dbip, gPath.c_str());
      db_free_full_path(&dfp);

      if (exists != 0) {
          /* failed */
          break;
      }

      gPath += "/";
      ++dbStep;
    }

  db_close(dbip);
  return dbStep + stackPos;
}

int
FileDataSource::pathToStringStack(std::string path, std::list<std::string>* stringStack)
{
  std::string sub = "";
  size_t endPos = 0;
  int cnt = 0;

  do {
      endPos = path.find_first_of(PATH_DELIM);
      if (endPos == std::string::npos){
          if (path.length() <=0)
              break;
          else
              endPos = path.length();
      }
      sub = path.substr(0, endPos);

      if (sub.length() > 0) {
        stringStack->push_back(sub);
        ++cnt;
      }
      path = path.erase(0, endPos+1);

  } while (endPos != std::string::npos);

  return cnt;
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
