/*                  J O B M A N A G E R . C X X
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

#include "JobManager.h"
#include "JobWorker.h"
#include "GSThread.h"

#include <iostream>

#include <QtCore/QMutexLocker>

//Declares for statics.
JobManager* JobManager::pInstance = NULL;
QMutex* JobManager::singletonLock = new QMutex();

JobManager::JobManager() {
	this->log = Logger::getInstance();
	this->jobQueue = new QList<AbstractJob*> ();
	this->queueLock = new QMutex();
	this->jobWorkers = new QList<JobWorker*> ();
	this->acceptJobs = false;

	char buf[BUFSIZ];
	std::string text;
	snprintf(buf, BUFSIZ, "Startup.  MAX_JOBWORKERS: %d", MAX_JOBWORKERS);
	text.assign(buf);

	this->log->logINFO("JobManager", text);

	for (quint32 i = 0; i < MAX_JOBWORKERS; ++i) {
		JobWorker* jw = new JobWorker();
		this->jobWorkers->append(jw);

		//text = "Created new JobWorker with ID of " + jw->getWorkerIdAsQString();
		//this->log->logINFO("JobManager", text);
	}

	snprintf(buf, BUFSIZ, "Created a total of %d JobWorkers", this->jobWorkers->size());
	text.assign(buf);
	this->log->logINFO("JobManager", text);
}

JobManager::~JobManager() {
	this->shutdown();

	delete jobQueue;
	//TODO Should I loop through jobs, destroying them as well?
	delete queueLock;

	delete jobWorkers;
}

void JobManager::startup() {

	if (this->jobWorkers->size() > 0)
		return;

	for (quint32 i = 0; i < this->jobWorkers->size(); ++i) {
		JobWorker* jw = this->jobWorkers->at(i);
		jw->start();
	}
	this->acceptJobs = true;
}

void JobManager::shutdown(bool finishJobQueue) {
	char buf[BUFSIZ];
	snprintf(buf, BUFSIZ, "JobManager Shutdown Requested. %d items in jobQueue remain...", this->jobQueue->size());
	this->log->logINFO("JobManager", buf);

	this->acceptJobs = false;

	//TODO These should be moved to preferences eventually.
	quint32 maxWaitTimeSecs = 60;
	quint32 waitTimePerLoopSecs = 5;
	quint32 maxPasses = (maxWaitTimeSecs / waitTimePerLoopSecs);
	quint32 curPasses = 0;

	while (this->jobQueue->size() != 0 && finishJobQueue) {
		snprintf(buf, BUFSIZ, "Waiting for JobWorkers to process JobQueue. %d items remain...", this->jobQueue->size());
		this->log->logINFO("JobManager", buf);
		GSThread::sleep(waitTimePerLoopSecs);
		++curPasses;
		if (curPasses >= maxPasses) {
			this->log->logINFO("JobManager",
					"Shutdown Wait-time fail safe reached.  Forcing Shutdown.");
			break;
		}
	}

	//loop through workers, shut them down individually. Then empty worker list
	while (!this->jobWorkers->isEmpty()) {
		JobWorker* jw = this->jobWorkers->front();
		if (jw != NULL) {
//			this->log->logINFO("JobManager", "Shutting Down JobWorker: " + jw->getWorkerId().toString());
			jw->shutdown();
		} else {
			this->log->logERROR("JobManager", "Null JobWorker.");
		}
		this->jobWorkers->pop_front();
	}

	snprintf(buf, BUFSIZ, "All JobWorkers Stopped. %d items in jobQueue remain...", this->jobQueue->size());
	this->log->logINFO("JobManager", buf);
}

JobManager* JobManager::getInstance() {
	QMutexLocker locker(JobManager::singletonLock);

	if (!JobManager::pInstance) {
		pInstance = new JobManager();
	}
	return JobManager::pInstance;
}

void JobManager::submitJob(AbstractJob* job) {
	if (this->acceptJobs == false) {
		this->log->logWARNING("JobManager",
				"Job not queued. JobWorkers are currently not working.");
		//TODO perhaps return a bool??
		return;
	}

	QMutexLocker locker(this->queueLock);
	this->jobQueue->append(job);
}

AbstractJob* JobManager::getNextJob() {
	QMutexLocker locker(this->queueLock);
	if (!this->jobQueue->isEmpty()) {
		return this->jobQueue->takeFirst();
	} else {
		return NULL;
	}
}

bool JobManager::hasNextJob() {
	QMutexLocker locker(this->queueLock);
	return !this->jobQueue->isEmpty();
}

quint32 JobManager::getWorkQueueLen() {
	QMutexLocker locker(this->queueLock);
	return this->jobQueue->size();
}

//TODO add JobWorker Monitor.

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
