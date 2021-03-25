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


#include <chrono>
#include <thread>

#include <iostream>
#include <list>

#include <GSThread.h>

// https://stackoverflow.com/a/10613664/2037687
#define sleep(c) std::this_thread::sleep_for(std::chrono::seconds(c));

//Declares for statics.
JobManager* JobManager::pInstance = NULL;
GSMutex* JobManager::singletonLock = new GSMutex();

JobManager::JobManager() {
	this->log = Logger::getInstance();
	this->jobQueue = new std::list<AbstractJob*> ();
	this->queueLock = new GSMutex();
	this->jobWorkers = new std::list<JobWorker*> ();
	this->acceptJobs = false;

	char buf[BUFSIZ];
	std::string text;
	snprintf(buf, BUFSIZ, "Startup.  MAX_JOBWORKERS: %d", MAX_JOBWORKERS);
	text.assign(buf);

	this->log->logINFO("JobManager", text);

	for (uint32_t i = 0; i < MAX_JOBWORKERS; ++i) {
		JobWorker* jw = new JobWorker();
		this->jobWorkers->push_back(jw);
	}

	snprintf(buf, BUFSIZ, "Created a total of %d JobWorkers", (int)this->jobWorkers->size());
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
	for (std::list<JobWorker*>::iterator it=this->jobWorkers->begin(); it != this->jobWorkers->end(); it++)
		(*it)->start();
	this->acceptJobs = true;
}

void JobManager::shutdown(bool finishJobQueue) {
	char buf[BUFSIZ];
	snprintf(buf, BUFSIZ, "JobManager Shutdown Requested. %d items in jobQueue remain...", (int)this->jobQueue->size());
	this->log->logINFO("JobManager", buf);

	this->acceptJobs = false;

	//TODO These should be moved to preferences eventually.
	uint32_t maxWaitTimeSecs = 60;
	uint32_t waitTimePerLoopSecs = 5;
	uint32_t maxPasses = (maxWaitTimeSecs / waitTimePerLoopSecs);
	uint32_t curPasses = 0;

	while (this->jobQueue->size() != 0 && finishJobQueue) {
		snprintf(buf, BUFSIZ, "Waiting for JobWorkers to process JobQueue. %d items remain...", (int)this->jobQueue->size());
		this->log->logINFO("JobManager", buf);
		sleep(waitTimePerLoopSecs);
		++curPasses;
		if (curPasses >= maxPasses) {
			this->log->logINFO("JobManager",
					"Shutdown Wait-time fail safe reached.  Forcing Shutdown.");
			break;
		}
	}

	//loop through workers, shut them down individually. Then empty worker list
	while (!this->jobWorkers->empty()) {
		JobWorker* jw = this->jobWorkers->front();
		if (jw != NULL) {
//			this->log->logINFO("JobManager", "Shutting Down JobWorker: " + jw->getWorkerId().toString());
			jw->shutdown();
		} else {
			this->log->logERROR("JobManager", "Null JobWorker.");
		}
		this->jobWorkers->pop_front();
	}

	snprintf(buf, BUFSIZ, "All JobWorkers Stopped. %d items in jobQueue remain...", (int)this->jobQueue->size());
	this->log->logINFO("JobManager", buf);
}

JobManager* JobManager::getInstance() {
	GSMutexLocker locker(JobManager::singletonLock);

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

	GSMutexLocker locker(this->queueLock);
	this->jobQueue->push_back(job);
}

AbstractJob* JobManager::getNextJob() {
	AbstractJob* j;
	GSMutexLocker locker(this->queueLock);
	if (!this->jobQueue->empty()) {
		j = this->jobQueue->front();
		this->jobQueue->pop_front();
		return j;
	} else {
		return NULL;
	}
}

bool JobManager::hasNextJob() {
	GSMutexLocker locker(this->queueLock);
	return !this->jobQueue->empty();
}

uint32_t JobManager::getWorkQueueLen() {
	GSMutexLocker locker(this->queueLock);
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
