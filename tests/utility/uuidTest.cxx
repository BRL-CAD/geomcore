/*                  U U I D T E S T . C X X
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
/** @file uuidTest.cxx
 *
 * Brief description
 *
 */

#include "GSUuid.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[])
{

	GSUuid* uuid = new GSUuid();
	printf("%s\n", uuid->toString()->c_str());
    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 4 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=4 tabstop=8
