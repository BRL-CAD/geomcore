#include "main.h"

using namespace Geometry;

#if 1
using namespace std;
int main()
{
  Utility::Timer x;

  // Do a big loop and report the elapsed time
  cout << "Timer started\n";
  x.start();
  for (int i = 0; i < 1000000000; ++i)
    ;
  cout << x.elapsed() << endl;

  // Restart the timer and do the loop again
  cout << "Timer reset\n";
  x.reset();
  for (int i = 0; i < 1000000000; ++i)
    ;
  cout << x.elapsed() << endl;

  // Stop the timer and repeat the loop
  cout << "Timer stopped\n";
  x.stop();
  for (int i = 0; i < 1000000000; ++i)
    ;
  cout << x.elapsed() << endl;

  // Do the loop again without restarting
  cout << "Timer started, not reset\n";
  x.start();
  for (int i = 0; i < 1000000000; ++i)
    ;
  cout << x.elapsed() << endl;

  return 0;
}
#else
int main(int argc, char *argv[])
{

  std::cout << "            triangle is: " << sizeof(Geometry::Triangle) << std::endl;
  std::cout << "     robust triangle is: " << sizeof(Geometry::RobustTriangle) << std::endl;
  std::cout << "traditional triangle is: " << sizeof(Geometry::TraditionalTriangle) << std::endl;
  std::cout << "    compact triangle is: " << sizeof(Geometry::CompactTriangle) << std::endl;

  RtApplication app = RtApplication(argc, argv);
  std::cout << "Build is from " << app.buildDate().string() << " at " << app.buildTime().string() << std::endl;
  app.run();

  return 0;

  std::string filename = "../../misc/ADS/DXF/CASTLE.DXF";
  Geometry::SceneFactory *factory = new Geometry::SceneFactory(filename);
  Scene* scene = factory->getScene();
  std::vector<Triangle*> primitives = scene->getGeometry();

  std::cout << "Found " << primitives.size() << " primitives in " <<  filename << std::endl;

  std::string filename2 = "../../misc/lrt/nff/tetra.nff";
  Geometry::SceneFactory *factory2 = new Geometry::SceneFactory(filename2);
  //  factory2->loadFromFile(filename2);
  Scene* scene2 = factory2->getScene();
  std::vector<Triangle*> primitives2 = scene2->getGeometry();

  std::cout << "Found " << primitives2.size() << " primitives in " << filename2 <<  std::endl;

  factory->loadFromFile("blahblah.foo");
  factory->loadFromFile(filename2);
  primitives = factory->getScene()->getGeometry();

  std::cout << "Found " << primitives.size() << " primitives total" <<  std::endl;


  Image::Pixel p;
  Image::PixelImage pi;

  return 0;
}
#endif
// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
