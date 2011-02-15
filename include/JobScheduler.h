/*                 J O B S C H E D U L E R . H
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
/** @file JobScheduler.h
 *
 * Utility for scheduling delayed one-shot,
 * repetitive, and delayed repetitive jobs.
 *
 */

#ifndef __JOBSCHEDULER_H__
#define __JOBSCHEDULER_H__

#include <GSThread.h>

class JobScheduler
{
public:
	static JobScheduler* getInstance();
	virtual ~JobScheduler();

private:
	static JobScheduler* pInstance;
	JobScheduler();

	/* Disable copy cstr and =operator */
	JobScheduler(JobScheduler const&){};
	JobScheduler& operator=(JobScheduler const&){};
};

#endif /*__JOBSCHEDULER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
