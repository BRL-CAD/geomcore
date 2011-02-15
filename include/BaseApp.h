/*                       B A S E A P P . H
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
/** @file BaseApp.h
 *
 * Brief description
 *
 */

#ifndef __BASEAPP_H__
#define __BASEAPP_H__

#include "libutility.h"

#include "GSThread.h"

class BaseApp : public GSThread
{
public:
    BaseApp();
    virtual ~BaseApp();

    void run();
    void stop(int exitCode = 0);

protected:
    Logger* log;

    /* Force subclasses to implement _run() */
    virtual int _run() = 0;

private:
	/* Disable copy cstr and =operator */
    BaseApp(BaseApp const&){};
    BaseApp& operator=(BaseApp const&){};
};

#endif /* __BASEAPP_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
