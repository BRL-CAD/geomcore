/*                      G S T H R E A D . H
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
/** @file GSThread.h
 *
 * Brief description
 *
 */

#ifndef __GSTHREAD_H__
#define __GSTHREAD_H__

#include <list>

#include <pthread.h>

class GSThread
{
    public:
	GSThread();
	~GSThread();

	void start();
	bool wait(int&);
	void terminate();
	bool isRunning();

	virtual void run() {};

    protected:

	/* For thread management */
	static std::list<GSThread*> threads;
	static void addThread(GSThread* thread);
	static void remThread(GSThread* thread);

    private:
	/* Disable copy cstr and =operator */
	GSThread(GSThread const&){};
	GSThread& operator=(GSThread const&){};

	static void* runner(void *);

	bool running;
	pthread_t p;
	int pthid;
};

class GSMutex
{
    public:
	GSMutex();
	~GSMutex();
	void lock();
	void unlock();
    private:
	pthread_mutex_t lck;
};

class GSMutexLocker
{
    public:
	GSMutexLocker(GSMutex*);
	~GSMutexLocker();
    private:
	GSMutex* mutex;
};

#endif /* __GSTHREAD_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
