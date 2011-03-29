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

#include "FileDataSource.h"
#include "brlcad/Object.h"
#include "brlcad/Arb8.h"

std::string testPathFile("/awesome/test.g");

void testGetters(FileDataSource* fds);

int main(int argc, char* argv[])
{
	std::cout << "Testing FDS at:" << testPathFile << "\n" << std::endl;

	FileDataSource* fds = new FileDataSource("./testdir");
	fds->init();

	testGetters(fds);


	std::cout << "Adding: ImAnArbEight" << std::endl;
	BRLCAD::Arb8 arb1(BRLCAD::Vector3D(1.0,1.0,1.0), BRLCAD::Vector3D(-1.0,-1.0,-1.0));
	arb1.SetName("ImAnArbEight");

	std::cout << "Adding: ImAnArbEight2" << std::endl;
	BRLCAD::Arb8 arb2;
	arb2.SetName("ImAnArbEight2");
	fds->putObj(testPathFile, arb2);
	fds->putObj(testPathFile, arb1);

	testGetters(fds);

	delete fds;
    return 0;
}

void testGetters(FileDataSource* fds)
{
	std::cout << "\nTesting getObj()" << std::endl;
	BRLCAD::Object* obj = fds->getObj(testPathFile);

	if (obj == NULL) {
		std::cout << "\treturned NULL" << std::endl;
	} else {
		std::cout << "\treturned a name: "<< obj->Name() << std::endl;
		obj->Destroy();
	}

	std::cout << "Testing getObjs()" << std::endl;
	std::list<BRLCAD::Object*>* objects = fds->getObjs(testPathFile);

	if (objects == NULL) {
		std::cout << "\treturned NULL" << std::endl;
	} else {
		for(std::list<BRLCAD::Object*>::iterator it = objects->begin();
			it != objects->end(); it++)
		{
			obj = *it;
			std::cout << "\t" << obj->Name() << std::endl;
		}
	}
	std::cout  << std::endl;
}


// Local Variables:
// mode: C++
// tab-width: 8
// c-basic-offset: 2
// indent-tabs-mode: t
// End:
// ex: shiftwidth=2 tabstop=8
