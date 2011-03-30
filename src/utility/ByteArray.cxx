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

#include <string>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ByteArray.h"

ByteArray::ByteArray() { bu_vlb_init(&vlb); }

/* Copy constructor */
ByteArray::ByteArray(ByteArray& original)
{
	bu_vlb_initialize(&vlb, original.size());
	bu_vlb_write(&vlb, (unsigned char*)original.data(), original.size());
}

ByteArray::ByteArray(const char *buf, int len)
{
	bu_vlb_initialize(&vlb, len);
	bu_vlb_write(&vlb, (unsigned char*) buf, len);
}

ByteArray::~ByteArray() {
	bu_vlb_free(&vlb);
}

char* ByteArray::data() { return (char*) bu_vlb_addr(&vlb); }
int ByteArray::size() { return bu_vlb_buflen(&vlb); }
void ByteArray::assign(const char* buf, int size)
{
	bu_vlb_reset(&vlb);
	bu_vlb_write(&vlb, (unsigned char*)buf, size);
}

void
ByteArray::printHexString(std::string prefix)
{
	size_t len = bu_vlb_buflen(&this->vlb);
	std::cout << prefix << " (Len: " << len << " bytes) ";

	unsigned char *p = bu_vlb_addr(&this->vlb);
	for (int i = 0; i<len;i++)
	{
		printf("%02x ", *p);
		p++;
	}
	std::cout << std::endl;
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
