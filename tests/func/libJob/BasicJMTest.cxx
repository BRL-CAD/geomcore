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
#include <chrono>
#include <thread>

// https://stackoverflow.com/a/10613664/2037687
#define sleep(c) std::this_thread::sleep_for(std::chrono::seconds(c));


#include "PrintToStdOutJob.h"
#include "JobManager.h"
#include "GSThread.h"

#define NUMJOBS 30

int main(int argc, char* argv[])
{
    JobManager* jm = JobManager::getInstance();
    jm->startup();

    //Prep array of jobs
    PrintToStdOutJob* jobs[NUMJOBS];
    for (int i = 0; i<NUMJOBS;++i)
    	jobs[i] = new PrintToStdOutJob("The quick brown fox jumps over the lazy dog.\n");

    for (int i = 0; i<NUMJOBS;++i)
    	jm->submitJob(jobs[i]);

    sleep(1);

    for (int i = 0; i<NUMJOBS;++i)
     	delete jobs[i];

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
