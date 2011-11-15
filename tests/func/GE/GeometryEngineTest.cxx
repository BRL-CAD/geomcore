/*         G E O M E T R Y E N G I N E T E S T . C X X
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
/** @file GeometryEngineTest.cxx
 *
 *
 */

#include <bu.h>
#include <raytrace.h>
#include <list>
#include "FileDataSource.h"

int
main(int argc, char* argv[])
{
  if (argc < 2)
    {
      std::cout << "Usage " << argv[0] << " BRLCAD-Database." << std::endl;
      return 1;
    }

  const char* gName = argv[1];
  std::cout << "Using: '" << gName << "' ." << std::endl;

  FileDataSource fds("./");

  int ret = 0;
  std::string testName(gName);

  std::list<std::string> listing;
  fds.getListing(testName, &listing);

  std::list<std::string>::iterator it;
  std::cout << "item count: " << listing.size() << std::endl;
  for (it = listing.begin(); it != listing.end(); ++it) {
      std::cout << "\t" << *it << std::endl;
  }

  return 0;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
