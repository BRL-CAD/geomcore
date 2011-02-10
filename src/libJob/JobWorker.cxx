/*                   J O B W O R K E R . C X X
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
/** @file JobWorker.cxx
 *
 * Brief description
 *
 */

#include "JobManager.h"

#include "JobWorker.h"
#include <iostream>

JobWorker::JobWorker()
{
    this->status = WORKER_NOTREADY;
    this->runCmd = true;
    this->workerId = QUuid::createUuid();
    this->log = Logger::getInstance();
}

JobWorker::~JobWorker()
{}

void JobWorker::run()
{
    JobManager* jm = JobManager::getInstance();

    this->status = WORKER_READY;

    //TODO There MUST be a more dynamic way to have the workers idle other than a sleep
    //NOTE Semaphore perhaps?  QT Signal?
    while (this->runCmd) {
    	this->status = WORKER_READY;
    	if (!jm->hasNextJob()) {
    		//TODO put the 100ms sleep into preferences, or make it dynamic.
    		this->msleep(100);

    	} else {
    		this->status = WORKER_WORKING;
    		AbstractJob* job = jm->getNextJob();

    		if (job == NULL) {
    			//TODO put in an error trap here.  jm->hasNextJob() returned TRUE, but the job returned was NULL?!
				continue;
    		}

    		JobResult result = job->doJob();

    		//TODO log the result of the job for debugging?
    	}
    	this->status = WORKER_READY;

    }
    this->status = WORKER_NOTREADY;
}

bool JobWorker::isIdle()
{
    return (this->status == WORKER_READY);
}

JobWorkerStatus JobWorker::getStatus()
{
    return this->status;
}

QUuid JobWorker::getWorkerId()
{
    return this->workerId;
}

std::string JobWorker::getWorkerIdAsString()
{
    return this->getWorkerId().toString().toStdString();
}

void JobWorker::shutdown()
{
    this->runCmd = false;
}

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
