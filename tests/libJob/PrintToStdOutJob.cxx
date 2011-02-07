/*            P R I N T T O S T D O U T J O B . C X X
 * ./src/libJob/PrintToStdOutJob.h
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
/** @file PrintToStdOutJob.cxx
 *
 */

#include "PrintToStdOutJob.h"
#include <iostream>
#include "libutility.h"

PrintToStdOutJob::PrintToStdOutJob(QString text)
{
    this->text = text;
    this->streamLock = new QMutex();
}

PrintToStdOutJob::~PrintToStdOutJob()
{
    delete this->streamLock;
}

JobResult PrintToStdOutJob::_doJob()
{
	GSThread::usleep(2500);
    QMutexLocker(this->streamLock);
    std::cout << "JobID:" << this->jobID << " Text: "<< text.toStdString();
    return JOB_COMPLETED_NO_ERRORS;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
