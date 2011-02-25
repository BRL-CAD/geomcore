/*              D A T A S T R E A M U T I L S . C X X
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
/** @file DataStreamUtils.cxx
 *
 * Brief description
 *
 */

#include "GSUuid.h"
#include "DataStreamUtils.h"

#include <arpa/inet.h>	/* ntohl */


GSUuid * 
DataStreamUtils::getGSUuid(DataStream* ds)
{
    return new GSUuid(DataStreamUtils::getString(ds));
}

void
DataStreamUtils::putGSUuid(DataStream* ds, GSUuid *uuid)
{
  DataStreamUtils::putString(ds, uuid->toString());
}

std::string* DataStreamUtils::getString(DataStream* ds)
{
  char *dsp;
  uint32_t len;
  std::string* out = new std::string();

  dsp = ds->getptr();
  len = ntohl(*(uint32_t *)dsp);

  std::cout << "Read String length of: " << len << std::endl;
  out->append(dsp+sizeof(uint32_t), len);
  ds->advance(len+sizeof(uint32_t));

  return out;
}

void DataStreamUtils::putString(DataStream* ds, std::string str)
{
  int l[1];

  *l = htonl((uint32_t)str.length());
  ds->append((const char *)l, 4);
  ds->append(str.c_str(), *l);
}

void DataStreamUtils::putString(DataStream* ds, std::string *str)
{
	DataStreamUtils::putString(ds, *str);
}

/*
void DataStreamUtils::printByteArray(ByteArray* ba) 
{
    uint32_t size = ba->size();
    char *b = ba->data();
    bu_log("\nByteArray.Size(): %d\n", size);

    for (uint32_t i = 0; i < size; ++i, b++) {
	bu_log("%d '%c' ", *b, *b);
	if ((i + 1) % 25 == 0)
	    bu_log("\n");
    }
    bu_log("\n");
}
*/

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
