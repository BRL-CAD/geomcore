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
#include "DataManager.h"

std::string testPathFile("/awesome/test.g");

int main(int argc, char* argv[])
{
	FileDataSource fds("./testRepo");

	std::cout << "Testing getObjs:\n";
	std::list<ExtObject*> objs;
	fds.getObjs(testPathFile, &objs, true);
	std::list<ExtObject*>::iterator it = objs.begin();

	ExtObject* moe = NULL;
	ExtObject* moeAfter = NULL;
	GeometryChunkMsg* chunk = NULL;

	while (it != objs.end()) {
		moe = *it;
		std::cout << "\t" << moe->getFullPath() << "\n";
		++it;

		moe->printObjState();

		//chunk = GeometryChunkMsg::objToChunk(moe);
		//moeAfter = GeometryChunkMsg::chunkToObj(chunk);
		std::cout << "\n";
		if (moeAfter == NULL)
			std::cout << "moeAfter was null\n";
		else
			moeAfter->printObjState();
	}

	return 0;
}

// Local Variables:
// mode: C++
// tab-width: 8
// c-basic-offset: 2
// indent-tabs-mode: t
// End:
// ex: shiftwidth=2 tabstop=8
