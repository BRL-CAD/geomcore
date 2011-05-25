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
#ifndef __STRINGUTILS_H__
#define __STRINGUTILS_H__

#include <string>
#include <list>

#define PATH_DELIM "/"
#define DOUBLE_PATH_DELIM "//"

class StringUtils {
public:
  virtual ~StringUtils();

  //TODO swap out with bu_file_type()??
  /**
     * Checks the supplied path for existence and whether its a file or directory.
     * return values:
     * 0 == does not exist
     * 1 == exists and is a Directory
     * 2 == exists and is a File
     */
  static const int isFileOrDir(const std::string path);

  /**
   * Splits the provided path 'path' into a std::list of path steps.
   * Steps are determined by scanning for PATH_DELIM and splitting at each instance.
   *
   * Returns the number of segments added to 'steps'
   */
  static const size_t splitPath(
      const std::string path,
      std::list<std::string>* steps);

  /**
   * Splits the provided string 'original' into a std::list of string segments.
   * Segments are determined by scanning for 'delimiter' and splitting at each instance.
   *
   * Returns the number of segments added to 'segments'
   */
  static const size_t splitString(
      const std::string original,
      std::list<std::string>* segments,
      const std::string delimiter);

  /**
   * Combines the left and right paths into combined using PATH_DELIM
   */
  static void combinePaths(
      std::string* combined,
      const std::string* left,
      const std::string* right);

  /**
   * Removes duplicate PATH_DELIM entries.
   */
  static void cleanString(std::string* out);

  /**
   * Walks 'path' and finds the first step that is a file.  'path' is then split at that point
   * into lPath and rPath respectively.  If a valid int* is provided, fn will set 'totalSteps'
   * equal to the number of steps that are in 'path' Note that both rPath and lPath will be
   * reinitialized to empty strings in this fn.
   *
   * Return value is the step at which 'path' was split.
   * If a path with no delimiters is passed in, this will return 0;
   * If a NULL lPath or rPath is passed in, this will return -1;
   */
  static const int splitPathAtFile(
      const std::string path,
      std::string* lPath,
      std::string* rPath,
      int* totalSteps);

  /**
   * Determines and returns the last step of the provided 'path'.
   * Steps are determined by PATH_DELIM.
   */
  static std::string getLastStepOfPath(const std::string path);


private:
  StringUtils();

};

#endif /* __STRINGUTILS_H__ */

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
