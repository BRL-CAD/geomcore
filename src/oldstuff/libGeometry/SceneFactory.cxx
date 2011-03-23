/*                S C E N E F A C T O R Y . C X X
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
/** @file SceneFactory.cxx
 *
 * Brief description
 *
 */

/* interface header */
#include "Geometry/SceneFactory.h"

/* standard library */
#include <algorithm>

/* external interface headers */
#include "libutility.h"

/* library-specific headers */
#include "NFFFileParser.h"
#include "DXFFileParser.h"

using namespace Geometry;

SceneFactory::SceneFactory(std::string filename) :
  _scene(new Scene)
{
  if (filename.size() != 0) {
    loadFromFile(filename);
  }
}

SceneFactory::~SceneFactory()
{
  delete _scene;
}

bool SceneFactory::loadFromFile(std::string filename)
{
  // filename should presently be at least X.EXT == size 5"
  if (filename.size() <= 5) {
    std::cerr << "filename is too short?" << std::endl;
    return false;
  }

  std::string extension = filename.substr(filename.size() - 4, 4);
  //  std::cout << "filename is " << filename << " and the extension is " << extension << std::endl;

  if (std::equal(extension.begin(), extension.end(), std::string(".dxf").begin(), SceneFactory::noCaseCompare)) {
    Utility::FileParser<DXFFileParser, Scene> *dxfparser = new Utility::FileParser<DXFFileParser, Scene>(filename);
    _scene->add(dxfparser->getContainer());

  } else if (std::equal(extension.begin(), extension.end(), std::string(".nff").begin(), SceneFactory::noCaseCompare)) {
    Utility::FileParser<NFFFileParser, Scene> *nffparser = new Utility::FileParser<NFFFileParser, Scene>(filename);
    _scene->add(nffparser->getContainer());

  } else {
    std::cout << "extension \"" << extension << "\" is not recognized" << std::endl;
    return false;
  }

  _filesLoaded.push_back(filename);

  return true;
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
