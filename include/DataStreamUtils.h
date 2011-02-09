/*                 D A T A S T R E A M U T I L S . H
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
/** @file DataStreamUtils.h
 *
 * Brief description
 *
 */

#ifndef __DATASTREAMUTILS_H__
#define __DATASTREAMUTILS_H__

#include <string>
#include <QtCore/QDataStream>
#include <QtCore/QUuid>
#include <iostream>

class DataStreamUtils
{
public:
	static std::string* getString(QDataStream* ds);
	static void putString(QDataStream* ds, std::string str);
	static QUuid* getQUuid(QDataStream* ds);
	static void putQUuid(QDataStream* ds, QUuid str);
	static void printQByteArray(QByteArray* ba);

private:
	DataStreamUtils(){};

	/* Disable copy cstr and =operator */
	DataStreamUtils(DataStreamUtils const&){};
	DataStreamUtils& operator=(DataStreamUtils const&){};
};

#endif /* __DATASTREAMUTILS_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
