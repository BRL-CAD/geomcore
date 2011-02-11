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

#include <arpa/inet.h>	/* ntohl */

#include "DataStreamUtils.h"

QUuid* 
DataStreamUtils::getQUuid(DataStream* ds)
{
  std::string* strUUID = DataStreamUtils::getString(ds);

  //std::cout << strUUID->toStdString();

  QUuid* out = new QUuid(strUUID->c_str());

  delete strUUID;

  return out;
}

void
DataStreamUtils::putQUuid(DataStream* ds, QUuid uuid)
{
  DataStreamUtils::putString(ds, uuid.toString().toStdString());
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

void DataStreamUtils::printByteArray(ByteArray* ba) 
{
  std::cout << std::endl;

  uint32_t size = ba->size();

  std::cout << "ByteArray.Size(): " << size << std::endl;

  for (uint32_t i = 0; i < size; ++i)
    {
      char c = ba->at(i);
      std::cout << (int)c << " '" << c << "', ";
      
      if ((i + 1) % 25 == 0) {
          std::cout << std::endl;
      }
    }
  std::cout << std::endl << std::endl;
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
