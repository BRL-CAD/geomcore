/*                   R O U T E M S G J O B . H
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
/** @file RouteMsgJob.h
 *
 * Brief description
 *
 */

#ifndef __ROUTEMSGJOB_H__
#define __ROUTEMSGJOB_H__

#include "NetMsg.h"
#include "AbstractJob.h"

class RouteMsgJob : public AbstractJob
{
public:
	RouteMsgJob(NetMsg* msg);
	virtual ~RouteMsgJob();

protected:
	JobResult _doJob();

private:
	NetMsg* msg;

	/* Disable copy cstr and =operator */
	RouteMsgJob(RouteMsgJob const&){};
	RouteMsgJob& operator=(RouteMsgJob const&){};
};

#endif /* __ROUTEMSGJOB_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
