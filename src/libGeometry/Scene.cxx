// interface header
#include "Geometry/Scene.h"

using namespace Geometry;

Scene::Scene()
{
}


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



// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
