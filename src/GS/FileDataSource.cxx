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
  FileDataSource::cleanString(&this->repoPath);
}

FileDataSource::~FileDataSource()
{}

/* get a directory listing or a child list */
int
FileDataSource::getListing(std::string path, std::list<std::string>* list)
{
  std::string absPath = "";
  FileDataSource::buildFullPath(&absPath, &this->repoPath, &path);

  int pathStep = 0;

  std::string fsPath;
  std::string gPath;
  int totalSteps = 0;
  int splitPoint = FileDataSource::splitPathAtFile(absPath, &fsPath, &gPath, &totalSteps);

  if (splitPoint == totalSteps) {
      /* We ended the path on a FS Dir or File */

      int type = FileDataSource::existsFileOrDir(fsPath.c_str());

      if (type <= 0)
          return -1; /* 0 == NOT EXIST */

      if (type == 1)
          return FileDataSource::getFsDirList(fsPath, list); /* 1 == DIR */

      /* Allow type == 2 (aka G File) to fall through to G procssing code. */
  }
  /* We ended in a .g so get the child list */
  std::string objName = "";

  /*  we are at tops of g file*/
  if (splitPoint == totalSteps)
      return FileDataSource::getGChildList(fsPath, objName, list);

  size_t found = gPath.rfind(PATH_DELIM);
  if (found == std::string::npos)
    return -1;
  objName = gPath.substr(found + 1);

  return FileDataSource::getGChildList(fsPath, objName, list);
}

int
FileDataSource::splitPathAtFile(std::string path, std::string* fsPath, std::string* gPath, int* totalSteps)
{
  std::string pathStep = "";
  int step = 0;
  int ford = 0;

  if (fsPath == NULL || gPath == NULL)
    return -1;

  (*fsPath) = "";
  (*gPath) = "";

  std::list<std::string> strStack;
  FileDataSource::pathToStringStack(path, &strStack);
  if (totalSteps != NULL)
    *totalSteps = strStack.size();

  if (strStack.size() == 0)
    return 0;

  std::list<std::string>::const_iterator it = strStack.begin();

  /* build FS string */
  for (; it != strStack.end(); ++it)
    {
      pathStep = (std::string)*it;
      *fsPath += pathStep;
      ford = FileDataSource::existsFileOrDir(fsPath->c_str());

      /* 2 == FILE, 1 == DIR, 0 == NOTEXIST */
      if (ford == 2) {
          ++step;
          break;
      } else if (ford == 1) {
          ++step;
          *fsPath += PATH_DELIM;
          continue;
      } else if (ford == 0) {
          return step * -1; /* Failed */
      } else {
          return step * -1; /* Failed */
      }
    }

  ++it;

  /* build FS string */
  for (; it != strStack.end(); ++it)
    {
      pathStep = (std::string)*it;
      if (it != strStack.end())
        *gPath += PATH_DELIM;
      *gPath += pathStep;

    }
  return step;
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
  Logger::getInstance()->logINFO("FileDataSource",
      this->repoPath + " is readable.");

  if (bu_file_writable(path) <= 0)
    return false;
  Logger::getInstance()->logINFO("FileDataSource",
      this->repoPath + " is writable.");

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
  *out = *base + PATH_DELIM + *path;
  FileDataSource::cleanString(out);
}

void
FileDataSource::cleanString(std::string* out)
{
  int pos = out->find(DOUBLE_PATH_DELIM);
  while (pos != -1)
    {
      out->replace(pos,2,PATH_DELIM);
      pos = out->find(DOUBLE_PATH_DELIM);
    }
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
FileDataSource::getGChildList(std::string gFilePath, std::string objName, std::list<std::string>* items, bool isTops)
{
  struct db_i *dbip;
  struct directory *dp;
  struct db_full_path dfp;
  int dbStep = 0;
  int exists = 0;

  /* Open DB file */
  if ((dbip = db_open(gFilePath.c_str(), "r")) == DBI_NULL) {
      Logger::getInstance()->logERROR("FileDataSource", "Unable to open geometry file " + gFilePath);
      return -1;
  }
  if (db_dirbuild(dbip)) {
      Logger::getInstance()->logERROR("FileDataSource", "ERROR: db_dirbuild failed");
      return -1;
  }

  db_update_nref(dbip, &rt_uniresource);

  /* If we are getting TOPS of the file... */
  if (isTops) {

    for (int i = 0; i < RT_DBNHASH; i++) {
      for (dp = dbip->dbi_Head[i]; dp != RT_DIR_NULL; dp = dp->d_forw) {
          /* Hide globals. */
        if (dp->d_nref == 0 && !(dp->d_flags & RT_DIR_HIDDEN) && (dp->d_addr != RT_DIR_PHONY_ADDR)) {
          items->push_back(std::string(dp->d_namep));
        }
      }
    }
    return 1;
  }

  dp = db_lookup(dbip, objName.c_str(), 0);
  if (dp == RT_DIR_NULL)
    {
      //Logger::getInstance()->logERROR("FileDataSource", "Directory was null when looking for: " + objName);
      return -1;
    }

  struct rt_db_internal in;
  struct rt_comb_internal *comb;

  if (rt_db_get_internal5(&in, dp, dbip, NULL, &rt_uniresource) < 0) {
      Logger::getInstance()->logERROR("FileDataSource", "rt_db_get_internal5 FAILED.");
      return -1;
  }

  comb = (struct rt_comb_internal *) in.idb_ptr;

  size_t i;
  size_t node_count;
  struct rt_tree_array *rt_tree_array;
  union tree *ntp;

  RT_CK_RESOURCE(&rt_uniresource);

  if (!comb->tree) {
      //Logger::getInstance()->logERROR("FileDataSource", "No Tree");
      return 1;
  }
  RT_CK_TREE(comb->tree);

  node_count = db_tree_nleaves(comb->tree);
  if (node_count == 0)
    {
      //Logger::getInstance()->logERROR("FileDataSource", "Zero node_count.");
      return 1;
    }

  ntp = db_dup_subtree(comb->tree, &rt_uniresource);
  RT_CK_TREE(ntp);

  /* Convert to "v4 / GIFT style", so that the flatten makes sense. */
  if (db_ck_v4gift_tree(ntp) < 0)
          db_non_union_push(ntp, &rt_uniresource);
  RT_CK_TREE(ntp);

  node_count = db_tree_nleaves(ntp);
  rt_tree_array = (struct rt_tree_array *) bu_calloc(node_count,
                  sizeof(struct rt_tree_array), "rt_tree_array");

  /*
   * free=0 means that the tree won't have any leaf nodes freed.
   */
  (void) db_flatten_tree(rt_tree_array, ntp, OP_UNION, 0, &rt_uniresource);

  union tree *itp = NULL;
  for (i = 0; i < node_count; i++)
    {
      itp = rt_tree_array[i].tl_tree;

      RT_CK_TREE(itp);
      BU_ASSERT_LONG(itp->tr_op, ==, OP_DB_LEAF);
      BU_ASSERT_PTR(itp->tr_l.tl_name, !=, NULL);

      items->push_back(std::string(itp->tr_l.tl_name));
    }

  if (rt_tree_array)
          bu_free((genptr_t) rt_tree_array, "rt_tree_array");
  db_free_tree(ntp, &rt_uniresource);

  rt_db_free_internal(&in);
  return 1;
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
