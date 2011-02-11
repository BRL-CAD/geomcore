/*		    D A T A S T R E A M . H
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
/** @file DataStream.h
 *
 * Brief description
 *
 */

#ifndef __DATASTREAM_H__
#define __DATASTREAM_H__

#include <stdio.h>

#include "ByteArray.h"

class DataStream
{
public:
	DataStream();
	DataStream(char* buf, int len);

	~DataStream();

	char *getptr();
	char *get(int);
	void advance(int);
	void append(const char *data, int len);
private:
	char *buf;
	int ind;
	int maxlen;
};

#endif /* __CONFIG_H__ */

/*
 * Local Variables: ***
 * mode: C++ ***
 * tab-width: 8 ***
 * c-basic-offset: 2 ***
 * indent-tabs-mode: t ***
 * End: ***
 * ex: shiftwidth=2 tabstop=8
*/
