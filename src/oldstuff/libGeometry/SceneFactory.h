/*                  S C E N E F A C T O R Y . H
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
/** @file SceneFactory.h
 *
 * Brief description
 *
 */

#ifndef __SCENEFACTORY_H__
#define __SCENEFACTORY_H__

/* system headers */
#include <iostream>
#include <vector>

/* interface headers */
#include "Scene.h"

namespace Geometry {
  class SceneFactory
  {
  private:
    Scene *_scene;

    static bool noCaseCompare(char c1, char c2) {
      return toupper(c1) == toupper(c2);
    }

  protected:
    std::vector<std::string> _filesLoaded;

  public:
    SceneFactory(std::string filename = "");
    ~SceneFactory();

    bool loadFromFile(std::string filename);

    Scene* getScene() {
      return _scene;
    }
  };
}

#endif /* __SCENEFACTORY_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
