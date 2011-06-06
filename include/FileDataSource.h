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
  bool init();

  /*
   * 'GET' ers
   */

  /* get a directory listing or a child list */
  int getListing(std::string path, std::list<std::string>* list);

  /**
   * Using provided database 'path' to a db object,
   * fill 'list' with the children objects
   *
   * Return value:
   *  >=0 number of children objs added to 'list'
   *  <0 error:
   *     -1 filesystem Path not valid.
   *     -2 geometry Path not valid.
   *     -3 Valid path, but corrupt Object Data.
   */
  int getObjs(std::string path, std::list<ExtObject*>* objs, bool recurse);

  /*
   * 'PUT' ers
   */

  /* Put a single BRLCAD::Object */
  bool putObj(std::string path, ExtObject* obj);

  static int getFsDirList(std::string, std::list<std::string>*);

  /**
   * Get a list of the names of the children for object 'gPath'
   * in the brlcad file 'fsPath'
   *
   * Return values:
   *      >=0 Number of items added to 'list'
   *      <0 error:
   *              -1 filesystem Path not valid.
   *              -2 geometry Path not valid.
   *              -3 Valid path, but corrupt Object Data.
   */
  static int getGChildList(
      std::string fsPath,
      std::string gPath,
      std::list<std::string>* list);

private:
  std::string repoPath;
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
