/*                     J O B W O R K E R . H
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
/** @file JobWorker.h
 *
 * Represents a single QThread implementation that is managed by the
 * JobManager and executes AbstractJob objects.
 *
 */

#ifndef __JOBWORKER_H__
#define __JOBWORKER_H__

//TODO Move this to preferences.
#define MAX_JOBWORKERS 5

#include <QtCore/QThread>
#include <QtCore/QUuid>
#include <string>

enum JobWorkerStatus {
	WORKER_NOTREADY, WORKER_READY, WORKER_WORKING
};

class JobWorker : public QThread {
public:
	/**
	 * Standard Constructor.  Instantiates a new JobWorker with a unique ID.
	 */
	JobWorker();
	virtual ~JobWorker();

	/**
	 * Override of QThread.run(). Starts JobWorker execution.
	 */
	void run();

	/**
	 * Returns the JobWorker's current status (WORKER_NOTREADY, WORKER_READY, WORKER_WORKING)
	 */
	JobWorkerStatus getStatus();

	/**
	 * Sets the JobWorker's runCmd flag to FALSE.
	 */
	void shutdown();

	/**
	 * Gets the UUID of the JobWorker.
	 */
	QUuid getWorkerId();

	/**
	 * Gets the UUID of the JobWorker as a QString.
	 */
	QString getWorkerIdAsQString();

	/**
	 * Gets the UUID of the JobWorker as a STL string.
	 */
	std::string getWorkerIdAsStdString();

	/**
	 * Returns TRUE if the JobWorker's status is currently WORKER_READY
	 */
	bool isIdle();

private:
	Logger* log;
	JobWorkerStatus status;
	bool runCmd;
	QUuid workerId;
};

#endif

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
