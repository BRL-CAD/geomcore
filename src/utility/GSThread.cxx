/*                    G S T H R E A D . C X X
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
/** @file GSThread.cxx
 *
 * Brief description
 *
 */

#include "GSThread.h"

#include <bu.h>

std::list<GSThread*> GSThread::threads;

GSThread::GSThread()
{
    this->running = false;
    this ->pthid = -1;
    GSThread::addThread(this);
}

GSThread::~GSThread()
{
    this->running = false;
    pthread_join(this->p, NULL);
    GSThread::remThread(this);
}

void
GSThread::terminate()
{
    this->running = false;
}

void
GSThread::addThread(GSThread* thread)
{
    GSThread::threads.push_back(thread);
}

void
GSThread::remThread(GSThread* thread)
{
    GSThread::threads.remove(thread);
}

bool
GSThread::wait(int& t)
{
}


void *
GSThread::runner(void *obj) {
    GSThread *me = (GSThread *)obj;
    me->run();
}

void
GSThread::start() 
{
    this->running = true;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    this->pthid = pthread_create(&this->p, &attr, runner, this);
    if(this->pthid < 0) {
	    bu_log("Crud: %d\n", this->pthid);
	    this->running = false;
    }
}

bool
GSThread::isRunning()
{
    return this->running;
}

GSMutex::GSMutex(){ pthread_mutex_init(&this->lck,NULL); }
GSMutex::~GSMutex(){}
void GSMutex::lock(){ pthread_mutex_lock(&this->lck);}
void GSMutex::unlock(){ pthread_mutex_unlock(&this->lck);}

GSMutexLocker::GSMutexLocker(GSMutex*mutex){ this->mutex=mutex; mutex->lock(); }
GSMutexLocker::~GSMutexLocker(){ mutex->unlock(); }

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
