/*              B A S I C E V E N T T E S T . C X X
 * BRLCAD
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
/** @file BasicEventTest.cxx
 *
 * Brief description
 *
 */

#include <brlcad/MemoryDatabase.h>

#include <brlcad/Halfspace.h>
#include <brlcad/Combination.h>


int main
(
    int   argc,
    char* argv[]
) {
    BRLCAD::Halfspace halfspace;
    halfspace.SetName("half.s");
    halfspace.SetNormal(BRLCAD::Vector3D(1., 1., 1.));
    halfspace.SetDistanceFromOrigin(1500.);

    BRLCAD::Combination region;
    region.SetName("half.r");
    region.SetIsRegion(true);
    region.AddLeaf("half.s");

    BRLCAD::Combination group;
    group.SetName("all.g");
    group.AddLeaf("half.r");

    BRLCAD::MemoryDatabase database;
    database.Add(halfspace);
    database.Add(region);
    database.Add(group);
    database.Save("my-db.g");

    return 0;
}


// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
