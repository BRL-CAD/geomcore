/*                       S C E N E . C X X
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
/** @file Scene.cxx
 *
 * Brief description
 *
 */

/* interface header */
#include "Geometry/Scene.h"

using namespace Geometry;

Scene::Scene()
{}

Scene::~Scene()
{
  // XXX delete the geometry and scene pointers
}

void Scene::add(Scene& scene)
{
  for (int i=0; i < scene._geometry.size(); i++) {
    _geometry.push_back(scene._geometry[i]);
  }
  //  std::cout << "geometry size in add Scene is " << scene._geometry.size() << std::endl;
  //  std::cout << "geometry size in add Scene is " << _geometry.size() << std::endl;
  for (int i=0; i < scene._views.size(); i++) {
    _views.push_back(scene._views[i]);
  }
}

void Scene::add(std::vector<Triangle*> geometry)
{
  for (int i=0; i < geometry.size(); i++) {
    _geometry.push_back(geometry[i]);
  }
  //  std::cout << "Added " << geometry.size() << " triangles" << std::endl;
  //  std::cout << "geometry size is " << _geometry.size() << std::endl;
}

void Scene::add(std::vector<View*> views)
{
  for (int i=0; i < views.size(); i++) {
    _views.push_back(views[i]);
  }
  //  std::cout << "Added " << views.size() << " views" << std::endl;
}

std::vector<Triangle*> Scene::getGeometry() const
{
  return _geometry;
}

std::vector<View*> Scene::getViews() const
{
  return _views;
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
