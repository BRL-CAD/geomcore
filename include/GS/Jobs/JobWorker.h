/*                     J O B W O R K E R . H
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
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
/** @file JobWorker.h
 *
 * Brief description
 *
 */

#ifndef __JOBWORKER_H__
#define __JOBWORKER_H__

#include "GS/GSCommon.h"
#include "GS/Jobs/AbstractJob.h"
#include <QThread>

class JobWorker: public QThread
{

public:
	JobWorker();
	virtual ~JobWorker();

	void run();

	JobWorkerStatus getStatus();
	void shutdown();
private:
	JobWorkerStatus status;
	bool runCmd;
};

#endif

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
