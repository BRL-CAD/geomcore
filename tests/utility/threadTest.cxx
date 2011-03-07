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

class Bah : public GSThread
{
    void run();
};

void Bah::run() {
    int i;
    pthread_t self;
    self = pthread_self();
    for(i = 0; i < 10; i++) {
	bu_log("%d:%d\n", self, i);
	usleep(rand()%0xf * 1000);
    }
}

int main(int argc, char* argv[])
{
    int i;
    Bah *b[5];
    for(i=0;i<5;i++) b[i] = new Bah();
    for(i=0;i<5;i++) b[i]->start();
    sleep(1);
    for(i=0;i<5;i++) b[i]->terminate();
    sleep(1);
    for(i=0;i<5;i++) delete b[i];

    return 0;
}

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 4 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=4 tabstop=8
