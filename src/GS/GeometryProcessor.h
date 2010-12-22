/*            G E O M E T E R Y P R O C E S S O R . H
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
/** @file GeometryProcessor.h
 *
 * Provides a single point of access for
 * manipulating Geometry. This is the prototype
 * interface to the Geometry Engine.
 *
 */

#ifndef __GEOMETRYPROCESSOR_H__
#define __GEOMETRYPROCESSOR_H__

class GeometryProcessor {
public:
	static GeometryProcessor* getInstance();
	virtual ~GeometryProcessor();

private:
	static GeometryProcessor* pInstance;

	GeometryProcessor();
};

#endif /* __GEOMETRYPROCESSOR_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
