/*                      B R L C A D D B . H
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
/** @file BrlcadDb.h
 *
 */

#ifndef __BRLCADDB_H__
#define __BRLCADDB_H__

#include "ExtObject.h"

#include <string>
#include <list>

class BrlcadDb {
public:
  //TODO move these to a more common place?
  static int FS_PATH_NOT_VALID;
  static int G_PATH_NOT_VALID;
  static int CORRUPT_OBJ_DATA;
  static int NULL_POINTER;

  /**
   * Factory method.  Returns valid BrlcadDb object on success, NULL on failure.
   */
  static BrlcadDb* makeDb(const std::string path);

  virtual ~BrlcadDb();

  /**
   * Opens the DB, checks if the provided 'path' is valid or not, then closes the DB.
   */
  const bool isValidPath(const std::string gPath);

  /**
   * Using provided database 'path' to a db object,
   * fill 'list' with the names children objects
   *
   * Return value:
   *  >=0 number of children objs added to 'list'
   *  <0 error:
   *     -1 filesystem Path not valid.
   *     -2 geometry Path not valid.
   *     -3 Valid path, but corrupt Object Data.
   */
  const int list(const std::string path, std::list<std::string>* list);


  /**
   * Verifies whether this DB contains the specified name.
   * This should be used to check names only.
   * To check a valid path, use isValidPath();
   */
  bool contains (const std::string name);

  /**
   * Builds an ExtObject if 'name' exists in this database
   */
  ExtObject* getExtObj(const std::string name);

  /**
   * Bulk build of ExtObjects from 'nameList' and place them into extList
   *
   * Return Values:
   *    0 >= Number of items added to extList;
   *    -1 = db_open() failed
   *    -2 = either nameList or extList was NULL
   */
  int getExtObjs(
      const std::list<std::string>* nameList,
      std::list<ExtObject*>* extList,
      bool recursive = false,
      std::string startPath = "");

private:
  std::string path;
  struct db_i* dbip;

  /**
   * Default constructor.  Accessed via BrlcadDb::makeDB to allow for file system
   * path validation and .g file validation.
   */
  BrlcadDb(const std::string path);

  /**
   * Opens the DB.  (db_open) Optional boolean argument for opening in ReadOnly mode.
   * Default is false, thus default is read/write mode.
   *
   * Return status is whether the db_open succeeded or not.
   */
  const bool open(const bool readOnly = false);

  /**
   * Closes the database. (db_close)
   */
  void close();

  /**
   * Performs the actual path validation. To be used internally only.
   * NOTE:  Does not call open() or close().  See isValidPath() that.
   */
  const bool _isValidPath(const std::string path);

  /**
   * Performs the actual list lookup. To be used internally only.
   * NOTE:  Does not call open() or close().  See list().
   */
  const int _list(const std::string path, std::list<std::string>* list);

   /**
     * Performs the actual name lookup. To be used internally only.
     * NOTE:  Does not call open() or close().  See contains().
     */
   bool _contains(const std::string name);

   /**
     * Performs the actual name lookup. To be used internally only.
     * NOTE:  Does not call open() or close().  See getExtObj().
     */
   ExtObject* _getExtObj(const std::string name);

   /**
      * Performs the actual name lookups. To be used internally only.
      * NOTE:  Does not call open() or close().  See getExtObjs().
      */
   int _getExtObjs(const std::list<std::string>* nameList,
       std::list<ExtObject*>* extList, bool recursive, std::string startPath);

};

#endif /* __BRLCADDB_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
