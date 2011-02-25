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
#include <arpa/inet.h>

#include "DataStream.h"

DataStream::DataStream() { bu_vlb_init(&vlb); ind=0; }
DataStream::DataStream(const char* data, int len) { bu_vlb_init(&vlb); bu_vlb_write(&vlb, (unsigned char *)data, len); ind=0; }
DataStream::~DataStream() { bu_vlb_free(&vlb); }

char* DataStream::getptr() { return (char *)bu_vlb_addr(&vlb); }
char* DataStream::get(int i) { char *t = (char *)bu_vlb_addr(&vlb)+ind; ind+=i; return t; }
void DataStream::advance(int i) { ind+=i; }
void DataStream::append(const char *data, int len) { bu_vlb_write(&vlb, (unsigned char *)data, len); }

std::string* DataStream::getString()
{
  char *dsp = getptr() + ind;
  uint32_t len;
  std::string* out = new std::string();

  len = ntohl(*(uint32_t *)dsp);
  dsp += sizeof(uint32_t);

  out->append(dsp, len);
  advance(len+sizeof(uint32_t));

  return out;
}

void DataStream::putString(std::string str)
{
  int l[1];

  *l = htonl((uint32_t)str.length());
  this->append((const char *)l, 4);
  this->append(str.c_str(), str.length());
}

void DataStream::putString(std::string *str)
{
	this->putString(*str);
}

int DataStream::size()
{
	return bu_vlb_buflen(&vlb);
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
