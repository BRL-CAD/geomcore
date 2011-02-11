/*                   B Y T E A R R A Y . C X X
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
/** @file ByteArray.cxx
 *
 * dynamic byte array
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ByteArray.h"

ByteArray::ByteArray()
{
    this->databuf = NULL;
    this->len = 0;
    this->maxlen = 0;
}

ByteArray::ByteArray(char *buf, int len) {
    this->len = len;
    this->maxlen = len;
    this->databuf = (char *)malloc(len);
    memcpy(databuf, buf, len);
}

ByteArray::~ByteArray()
{
    delete this->databuf;
}

char *ByteArray::data() { return databuf; }
int ByteArray::size() { return len; }
int ByteArray::length() { return len; }
char ByteArray::at(int i) { return databuf[i]; }

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
