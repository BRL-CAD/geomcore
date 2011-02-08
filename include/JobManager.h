/*                    J O B M A N A G E R . H
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
/** @file JobManager.h
 *
 * Singleton class.  Managers all JobWorkers, and their task allocation.  Maintains a FIFO AbstractJob queue.
 *
 */

#ifndef __JOBMANAGER_H__
#define __JOBMANAGER_H__

#include "AbstractJob.h"
#include "Logger.h"

#include <QtCore/QList>
#include <QtCore/QMutex>

//Forward Decl
class JobWorker;

class JobManager
{
public:
	/**
	 * Provides controlled access to the singleton JobManager object.
	 */
	static JobManager* getInstance();

	virtual ~JobManager();

	/**
	 * Returns the next AbstractJob in the JobManager's internal FIFO Queue.  If empty, returns NULL.
	 */
	AbstractJob* getNextJob();

	/**
	 * Returns a boolean value indicating whether or not the JobManager's internal FIFO Queue contains an AbstractJob that is waiting for execution.
	 */
	bool hasNextJob();

	/**
	 * Returns the JobManager's internal FIFO Queue length.
	 */
	quint32 getWorkQueueLen();

	/**
	 * Places AbstractJob <i>aj</i> into the JobManager's internal FIFO Queue.  If the JobManager is
	 * currently not accepting AbstractJobs, a warning is issued via the Logging system.
	 */
	void submitJob(AbstractJob* aj);

	/**
	 * Starts all the JobWorkers that were initialized in the JobManager constructor.
	 */
	void startup();

	/**
	 * Sets the JobManager's AcceptJobs flag to false and shuts down all the JobWorkers associated
	 * with the JobManager.  Optionally, this function will block until all AbstractJobs in the JobManager's
	 * internal FIFO Queue are executed.  There is a fail-safe built into the optional wait that will prevent a
	 * stalled job from preventing a JobManager.  Should the fail-safe be tripped a Log entry will be made.
	 */
	void shutdown(bool finishJobQueue = true);

private:
	static JobManager* pInstance;
	JobManager();
	static QMutex* singletonLock;

	QList<JobWorker*>* jobWorkers;
	QList<AbstractJob*>* jobQueue;
	QMutex* queueLock;
	Logger* log;

	bool acceptJobs;

	/* Disable copy cstr and =operator */
	JobManager(JobManager const&){};
	JobManager& operator=(JobManager const&){};
};

#endif /* __JOBMANAGER_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
