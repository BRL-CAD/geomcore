/*                 S T R I N G U T I L S . C X X
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
/** @file StringUtils.cxx
 *
 */

#include "StringUtils.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>

const int
StringUtils::isFileOrDir(const std::string path)
{
  struct stat st_buf;

  /* Check existence */
  if ((stat(path.c_str(), &st_buf)) != 0)
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

const size_t
StringUtils::splitPath(const std::string path, std::list<std::string>* steps)
{
  return StringUtils::splitString(path, steps, PATH_DELIM);
}

const size_t
StringUtils::splitString(const std::string original, std::list<std::string>* segments, const std::string delimiter)
{
  std::string sub = "";
  std::string copy = original;
  size_t endPos = 0;
  size_t cnt = 0;

  do {
      endPos = copy.find_first_of(delimiter);
      if (endPos == std::string::npos){
          if (copy.length() <=0)
              break;
          else
              endPos = copy.length();
      }
      sub = copy.substr(0, endPos);

      if (sub.length() > 0) {
          segments->push_back(sub);
        ++cnt;
      }
      copy = copy.erase(0, endPos+1);

  } while (endPos != std::string::npos);

  return cnt;
}

void
StringUtils::combinePaths(
    std::string* combined,
    const std::string* left,
    const std::string* right
    )
{
  *combined = *left + PATH_DELIM + *right;
  StringUtils::cleanString(combined);
}

void
StringUtils::cleanString(std::string* out)
{
  int pos = out->find(DOUBLE_PATH_DELIM);
  while (pos != -1)
    {
      out->replace(pos,2,PATH_DELIM);
      pos = out->find(DOUBLE_PATH_DELIM);
    }
}

const int
StringUtils::splitPathAtFile(const std::string path, std::string* lPath, std::string* rPath, int* totalSteps)
{
  std::string pathStep = "";
  int step = 0;
  int ford = 0;

  if (lPath == NULL || rPath == NULL)
    return -1;

  (*lPath) = "";
  (*rPath) = "";

  std::list<std::string> strStack;
  StringUtils::splitPath(path, &strStack);

  if (totalSteps != NULL)
    *totalSteps = strStack.size();

  if (strStack.size() == 0)
    return 0;

  std::list<std::string>::const_iterator it = strStack.begin();

  /* build FS string */
  for (; it != strStack.end(); ++it)
    {
      pathStep = (std::string)*it;
      *lPath += pathStep;
      ford = StringUtils::isFileOrDir(lPath->c_str());

      /* 2 == FILE, 1 == DIR, 0 == NOTEXIST */
      if (ford == 2) {
          ++step;
          break;
      } else if (ford == 1) {
          ++step;
          *lPath += PATH_DELIM;
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
        *rPath += PATH_DELIM;
      *rPath += pathStep;

    }
  return step;
}


// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
