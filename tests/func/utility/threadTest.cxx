/*                  T H R E A D T E S T . C X X
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
/** @file threadTest.cxx
 *
 * Brief description
 *
 */

#include "GSThread.h"

#include <bu.h>
#include <unistd.h>

#include <list>

#define ATTEMPT(func,name) bu_log("%s\t[\x1B[3%s\x1B[m]\n", name, func()?"1mFAILED":"2mPASSED")

/**************** GSThread Test Section ************************/
std::list<int> *resbucket;
GSMutex *mut;

class Bah : public GSThread
{
    void run() {
	for(int i = 0; i < 10; i++) {
	    mut->lock();
	    resbucket->push_back(i);
	    mut->unlock();
	    usleep(rand()%0xf * 1000);
	}
    }
};

int
testGSThread()
{
    int i, prev;
    Bah *b[5];

    resbucket = new std::list<int>();
    mut = new GSMutex();

    /* throw threads to fill the list */
    for(i=0;i<5;i++) b[i] = new Bah();
    for(i=0;i<5;i++) b[i]->start();
    sleep(1);
    for(i=0;i<5;i++) b[i]->terminate();
    usleep(10 * (2 + 0xf) * 1000);	/* TODO: spinloop the list cleaning finished threads until empty? */
    for(i=0;i<5;i++) delete b[i];

    /* verify the results */
    if(resbucket->size() != 5*10) {
	bu_log("result bucket is not 50, but %d!\n", (int)resbucket->size());
	return -1;
    }

    /* make sure the list is NOT sequential */
    std::list<int>::iterator it = resbucket->begin();
    prev = *it;
    while(++it != resbucket->end()) {
	/* if not sequential, then insertions were probably threaded */
	if(*it < prev) {
	    delete resbucket;
	    delete mut;
	    return 0;
	}
	prev = *it;
    }

    delete resbucket;
    delete mut;

    bu_log("All elements sequentional, no threading?\n");

    return -1;
}

/*************** Controlled Thread Test Section ***************/

int
testControlledThread()
{
    return -1;
}

/*************** main driver ***************/
int main(int argc, char* argv[])
{
    ATTEMPT(testGSThread,         "GSThread        ");
    ATTEMPT(testControlledThread, "ControlledThread");

    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 4 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=4 tabstop=8
