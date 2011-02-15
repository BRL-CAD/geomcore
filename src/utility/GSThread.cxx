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

std::list<GSThread*> GSThread::threads;

GSThread::GSThread()
{
  GSThread::addThread(this);
}

GSThread::~GSThread()
{
  GSThread::remThread(this);
}

void
GSThread::terminate()
{
	QThread::terminate();
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

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
