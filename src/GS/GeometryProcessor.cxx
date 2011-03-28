/*            G E O M E T E R Y P R O C E S S O R . C X X
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
/** @file GeometryProcessor.cxx
 *
 * Provides a single point of access for
 * manipulating Geometry. This is the prototype
 * interface to the Geometry Engine.
 *
 */

#include "GeometryProcessor.h"
#include <GSThread.h>

GeometryProcessor* GeometryProcessor::pInstance = NULL;

GeometryProcessor::GeometryProcessor() {}

GeometryProcessor::~GeometryProcessor() {}

GeometryProcessor*
GeometryProcessor::getInstance() {
    if (!GeometryProcessor::pInstance)
	pInstance = new GeometryProcessor();
    return GeometryProcessor::pInstance;
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
