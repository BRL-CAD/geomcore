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

ByteArray::ByteArray() { bu_vlb_init(&vlb); }
ByteArray::ByteArray(char *buf, int len) { bu_vlb_initialize(&vlb, len); bu_vlb_write(&vlb, (unsigned char *)buf, len); }
ByteArray::~ByteArray() { bu_vlb_free(&vlb); }

char *ByteArray::data() { return (char *)bu_vlb_addr(&vlb); }
int ByteArray::size() { return bu_vlb_buflen(&vlb); }

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
