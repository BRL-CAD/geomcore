/*                      D B O B J E C T . H
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
/** @file DbObject.h
 * DbObject.h
 *
 *  Created on: Sep 29, 2010
 *      
 */

#ifndef __DBOBJECT_H__
#define __DBOBJECT_H__

#include <string>
#include <QtCore/QUuid>
#include <ByteArray.h>

class DbObject
{
public:
	DbObject(std::string path, ByteArray* data);
	DbObject(QUuid id, ByteArray* data);
	virtual ~DbObject();

	std::string getPath();
	QUuid getID();
	ByteArray* getData();

private:
	std::string path;
	QUuid id;
	ByteArray* data;

	/* Disable copy cstr and =operator */
	DbObject(DbObject const&){};
	DbObject& operator=(DbObject const&){};
};

#endif /* __DBOBJECT_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
