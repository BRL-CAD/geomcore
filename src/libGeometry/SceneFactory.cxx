// interface header
#include "Geometry/SceneFactory.h"

// standard library
#include <algorithm>

// external interface headers
#include "Utility/Utility.h"

// library-specific headers
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


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
