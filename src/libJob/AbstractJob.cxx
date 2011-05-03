/*                 A B S T R A C T J O B . C X X
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
/** @file AbstractJob.cxx
 *
 * Brief description
 *
 */

#include "AbstractJob.h"
#include "JobManager.h"

AbstractJob::AbstractJob()
{
    this->status = JOB_NOTSTARTED;
    this->jobID = GSUuid::createUuid();
}

AbstractJob::~AbstractJob()
{}

JobResult AbstractJob::doJob()
{
    this->status = JOB_RUNNING;
    JobResult retVal = this->_doJob();
    this->status = JOB_FINISHED;
    return retVal;
}

JobStatus AbstractJob::getStatus()
{
    return this->status;
}

GSUuid* AbstractJob::getJobId()
{
    return this->jobID;
}

void AbstractJob::submit()
{
    JobManager::getInstance()->submitJob(this);
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
