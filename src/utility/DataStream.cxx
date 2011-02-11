/*                  D A T A S T R E A M . C X X
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
/** @file DataStream.cxx
 *
 * data stream.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "DataStream.h"

DataStream::DataStream() { buf = NULL, ind = 0, maxlen = 0; }
DataStream::DataStream(char* data, int len) {
    buf = (char *)malloc(len);
    memcpy(buf, data, len);
    ind = 0;
    maxlen = len;
}
DataStream::~DataStream() { if(buf)free(buf); }

char* DataStream::getptr() { return buf; }
char* DataStream::get(int i) { char *t = buf+ind; ind+=i; return t; }
void DataStream::advance(int i) { ind+=i; }
void DataStream::append(const char *data, int len) {
    if(ind + len > maxlen)
	buf = (char *)realloc(buf, maxlen += len>BUFSIZ?len:BUFSIZ);
    memcpy(buf+ind, data, len);
    ind+=len;
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
