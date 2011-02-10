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

#include <QtCore/QThread>
#include <list>

class GSThread : public QThread
{
public:
  GSThread();
  virtual ~GSThread();

  static void sleep (unsigned long secs);
  static void msleep (unsigned long msecs);
  static void usleep (unsigned long usecs);

  void terminate();

protected:

  /* For thread management */
  static std::list<GSThread*> threads;
  static void addThread(GSThread* thread);
  static void remThread(GSThread* thread);

private:
	/* Disable copy cstr and =operator */
	GSThread(GSThread const&){};
	GSThread& operator=(GSThread const&){};
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
