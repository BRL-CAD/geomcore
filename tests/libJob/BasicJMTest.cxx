/*                 B A S I C J M T E S T . C X X
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
/** @file BasicJMTest.cxx
 *
 * Brief description
 *
 */

#include "PrintToStdOutJob.h"
#include "JobManager.h"
#include "GSThread.h"

int main(int argc, char* argv[])
{
    PrintToStdOutJob* ptsoJob = new PrintToStdOutJob("The quick brown fox jumps over the lazy dog.\n");

    JobManager* jm = JobManager::getInstance();
    jm->startup();

    for (int i = 0; i< 30;++i) {
	jm->submitJob(ptsoJob);
	usleep(100);
    }

    delete jm;
    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
