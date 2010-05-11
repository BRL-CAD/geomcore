/*                  J O B M A N A G E R . C X X
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
/** @file JobManager.cxx
 *
 * Singleton.
 *
 */

#include "JobManager.h"
#include "JobWorker.h"
#include <QMutexLocker>
#include <iostream>

JobManager* JobManager::pInstance = NULL;
QMutex* JobManager::singletonLock = new QMutex();

JobManager::JobManager()
{
    this->log = Logger::getInstance();
    this->jobQueue = new QList<AbstractJob*> ();
    this->queueLock = new QMutex();
    this->jobWorkers = new QList<JobWorker*> ();
    this->acceptJobs = false;

    QString text = "Startup.  MAX_JOBWORKERS: " + QString::number(
	    MAX_JOBWORKERS);

    this->log->logINFO("JobManager", text);

    for (quint32 i = 0; i < MAX_JOBWORKERS; ++i) {
	JobWorker* jw = new JobWorker();
	this->jobWorkers->append(jw);

	text = "Created new JobWorker with ID of " + jw->getWorkerIdAsQString();
	this->log->logINFO("JobManager", text);
    }

    text = "Created a total of " + QString::number(this->jobWorkers->size()) + " JobWorkers";
    this->log->logINFO("JobManager", text);
}

JobManager::~JobManager()
{
    this->shutdown();

    delete jobQueue;
    //TODO Should I loop through jobs, destroying them as well?
    delete queueLock;

    delete jobWorkers;
}
void JobManager::startup()
{
    for (quint32 i = 0; i < MAX_JOBWORKERS; ++i) {
 	JobWorker* jw = this->jobWorkers->at(i);
 	jw->start();
     }
    this->acceptJobs = true;
}

void JobManager::shutdown(bool finishJobQueue)
{
    this->log->logINFO("JobManager", "JobManager Shutdown Requested. " + QString::number(this->jobQueue->size()) + " items in jobQueue remain...");

    this->acceptJobs = false;

    //TODO These should be moved to preferences eventually.
    quint32 maxWaitTimeSecs = 60;
    quint32 waitTimePerLoopSecs = 5;
    quint32 maxPasses = maxWaitTimeSecs / waitTimePerLoopSecs;
    quint32 curPasses = 0;

    while (this->jobQueue->size() != 0 && finishJobQueue) {
	this->log->logINFO("JobManager", "Waiting for JobWorkers to process JobQueue. " + QString::number(this->jobQueue->size()) + " items remain...");
	ThreadUtils::sleep(waitTimePerLoopSecs);
	++curPasses;
	if (curPasses >= maxPasses) {
	    this->log->logINFO("JobManager", "Shutdown Wait time failsafe tripped.  Forcing Shutdown.");
	    break;
	}
    }

    //loop through workers, shut them down individually. Then empty worker list
    while (!this->jobWorkers->isEmpty()) {
	JobWorker* jw = this->jobWorkers->front();
	jw->shutdown();
	this->jobWorkers->pop_front();
    }

    this->log->logINFO("JobManager", "All JobWorkers Stopped. " + QString::number(this->jobQueue->size()) + " items in jobQueue remain...");
}

JobManager* JobManager::getInstance()
{
    QMutexLocker locker(JobManager::singletonLock);

    if (!JobManager::pInstance) {
	pInstance = new JobManager();
    }
    return JobManager::pInstance;
}

void JobManager::submitJob(AbstractJob* job)
{
    if (this->acceptJobs == false) {
	log->logWARNING("JobManager", "Job not queued. JobWorkers are currently not working.");
	return; //TODO perhaps return a bool??
    }

    QMutexLocker locker(this->queueLock);
    this->jobQueue->append(job);
}

AbstractJob* JobManager::getNextJob()
{
    QMutexLocker locker(this->queueLock);
    if (!this->jobQueue->isEmpty()) {
	return this->jobQueue->takeFirst();
    }
    else {
	return NULL;
    }
}

bool JobManager::hasJobsToWork()
{
    QMutexLocker locker(this->queueLock);
    return !this->jobQueue->isEmpty();
}

quint32 JobManager::getWorkQueueLen()
{
    return this->jobQueue->size();
}

//TODO add JobWorker Monitor.

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
