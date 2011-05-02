/*                  B Y T E B U F F E R . C X X
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
/** @file ByteBuffer.cxx
 * ByteBuffer.cxx
 *
 */

#include "ByteBuffer.h"
#include <netinet/in.h>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string.h>

const uint32_t ByteBuffer::defaultBufferSize = 1024 * 4;


ByteBuffer::ByteBuffer(size_t size)
{
  bu_vlb_initialize(&vlb, size);
  this->setLimit(this->capacity());
  this->setPosition(0);
  this->setMark(-1);
}

ByteBuffer::~ByteBuffer()
{
  bu_vlb_free(&vlb);
}

ByteBuffer*
ByteBuffer::allocate(size_t size)
{
  if (size < 1)
    return NULL;

  ByteBuffer* bb = new ByteBuffer(size);
  return bb;
}

ByteBuffer*
ByteBuffer::wrap(char* data, size_t size)
{
  if (size < 1)
    return NULL;

  if (data == NULL)
    data = (char*) bu_malloc(size, "BB.get()");

  ByteBuffer* bb = ByteBuffer::allocate(size);

//  memcpy(bb->vlb.buf, data, size);
  if (bb->put(data,size) == false)
    std::cout << "put failed.\n";
  return bb;
}

ByteBuffer*
ByteBuffer::duplicate()
{
  ByteBuffer* bb = ByteBuffer::allocate(this->capacity());

  memcpy(bb->vlb.buf, this->vlb.buf, this->vlb.bufCapacity);
  bb->vlb.bufCapacity = this->vlb.bufCapacity;
  bb->vlb.nextByte = this->vlb.nextByte;
  bb->setMark(this->getMark());
  bb->setLimit(this->limit());
  return bb;
}

char
ByteBuffer::get()
{

  char c = 0;
  this->get(&c, 1);
  return c;
}

void
ByteBuffer::get(char* data, size_t length)
{
  if (length < 1)
    return;

  if (length + this->position() > this->limit())
    return;

  if (data == NULL)
    data = (char*) bu_malloc(length, "BB.get()");

  BU_ASSERT((this->position() + 1 <= this->limit()) && "Buffer Overrun.");

  char* old = this->array();
  old += this->position();

  memmove(data, old, length);
  this->setPosition(this->position() + length);
}

bool
ByteBuffer::put(char c)
{
  return this->put(&c, 1);
}

bool
ByteBuffer::put(ByteBuffer* src)
{
  /* Cannot put self into self */
  if (this == src)
    return false;

  size_t rem = src->remaining();
  char* old = src->array();
  old += src->position();

  return this->put(old, rem);
}

bool
ByteBuffer::put(char* c, size_t length)
{
  bu_vlb_write(&vlb, (unsigned char*) c, length);

  if (this->position() > this->limit())
    this->setLimit(this->position());

  return true; //TODO Do we really need to return a bool?
}

char*
ByteBuffer::array()
{
  return (char*) this->vlb.buf;
}

void
ByteBuffer::compact()
{
  // limit - position
  size_t rem = this->remaining();

  char* old = (char*) this->vlb.buf;

  /*  use memmove due to potential overlap */
  memmove(this->vlb.buf, this->vlb.buf + this->position(), rem);

  this->setLimit(this->capacity());
  this->setPosition(rem);
  this->discardMark();
}

std::string
ByteBuffer::toString()
{
  std::string out("");

  out += "ByteBuffer ";
  out += "[pos=";
  out += this->position();
  out += " lim=";
  out += this->limit();
  out += " cap=";
  out += this->capacity();
  out += "]";
  return out;
}

std::string
ByteBuffer::toHexString(bool printToPosition)
{
  std::ostringstream ss;

  uint32_t len = 0;
  if (printToPosition){
      len = this->position();
      ss << " (Position: " << len << " bytes) ";
  } else {
      len = this->capacity();
      ss << " (Capacity: " << len << " bytes) ";
  }

  if (len == 0)
    return "<Empty>";

  char* p = this->array();
  unsigned int b;
  for (int i = 0; i<len;i++)
  {
      b = (unsigned int)*p;
      if (b < 10)
        ss << std::hex << '0' << (unsigned int)*p << ' ';
      else
        ss << std::hex << (unsigned int)*p << ' ';
      p++;
  }

  return ss.str();
}

uint16_t
ByteBuffer::get16bit()
{
  BU_ASSERT((this->position() + 2 <= this->limit()) && "Buffer Overrun.");

  char* ptr = (char*)bu_vlb_addr(&this->vlb) + this->position();

  uint16_t net = *(uint16_t*)ptr;
  uint16_t host = ntohs(net);
  this->setPosition(this->position() + 2);

  return host;
}

void
ByteBuffer::put16bit(uint16_t host)
{
  uint16_t net = htons(host);

  /* this call also advances position */
  bu_vlb_write(&this->vlb, (unsigned char *)&net, 2);
  if (this->position() > this->limit())
    this->setLimit(this->position());
}

uint32_t
ByteBuffer::get32bit()
{
  BU_ASSERT((this->position() + 4 <= this->limit()) && "Buffer Overrun.");

  char* ptr = (char*)bu_vlb_addr(&this->vlb) + this->position();

  uint32_t net = *(uint32_t*)ptr;
  uint32_t host = ntohl(net);
  this->setPosition(this->position() + 4);

  return host;
}

void
ByteBuffer::put32bit(uint32_t host)
{
  uint32_t net = htonl(host);

  /* this call also advances position */
  bu_vlb_write(&this->vlb, (unsigned char *)&net, 4);

  if (this->position() > this->limit())
    this->setLimit(this->position());
}

uint64_t
ByteBuffer::get64bit()
{
  BU_ASSERT((this->position() + 8 <= this->limit()) && "Buffer Overrun.");

  char* ptr = (char*)bu_vlb_addr(&this->vlb) + this->position();

  uint64_t net = *(uint64_t*)ptr;
  uint64_t host = ntohll(net);
  this->setPosition(this->position() + 8);
  return host;
}

void
ByteBuffer::put64bit(uint64_t host)
{
  uint64_t net = htonll(host);

  /* this call also advances position */
  bu_vlb_write(&this->vlb, (unsigned char *)&net, 8);

  if (this->position() > this->limit())
    this->setLimit(this->position());
}


void
ByteBuffer::putString(std::string str)
{
  this->put32bit(str.length());
  this->put((char*)str.c_str(), str.length());
}

std::string
ByteBuffer::getString()
{
  uint32_t len = this->get32bit();

  char* ptr = this->array() + this->position();
  this->setPosition(this->position() + len);
  std::string out = "";
  out.append(ptr, len);

/*
  std::cout << "pos: " << this->position();
  std::cout << "/" << this->capacity();
  std::cout << "len: " << len << " string: '";
  std::cout << out << "'\n";
*/

  return out;
}

size_t
ByteBuffer::remaining()
{
  return this->limit() - this->position();
}

size_t
ByteBuffer::capacity()
{
  return this->vlb.bufCapacity;
}

size_t
ByteBuffer::position()
{
  return this->vlb.nextByte;
}

bool
ByteBuffer::setPosition(size_t newPosition)
{
  if ((newPosition > this->limit()) || (newPosition < 0))
    return false;
  this->vlb.nextByte = newPosition;
  if (this->getMark() > this->position())
    this->discardMark();
  return true;
}

size_t
ByteBuffer::limit()
{
  return this->lim;
}

bool
ByteBuffer::setLimit(size_t newLimit)
{
  if ((newLimit > this->capacity()) || (newLimit < 0))
    return false;

  if (this->position() > newLimit)
    this->setPosition(newLimit);

  if (this->getMark() > newLimit)
    this->discardMark();

  this->lim = newLimit;
  return true;
}

bool
ByteBuffer::setMark(ssize_t m)
{
  if (m < -1)
    return false;

  if (m > (ssize_t)this->limit())
    return false;

  this->mar = m;
  return true;
}

ssize_t
ByteBuffer::getMark()
{
  return this->mar;
}

ssize_t
ByteBuffer::mark()
{
  this->setMark(this->position());
  return this->getMark();
}

void
ByteBuffer::discardMark()
{
  this->setMark(-1);
}

bool
ByteBuffer::reset()
{
  ssize_t m = this->getMark();
  if (m < 0)
    return false;

  if (m > (ssize_t)this->limit())
    return false;

  if (m > this->capacity())
    return false;

  this->setPosition(this->getMark());

  return true;
}

void
ByteBuffer::clear()
{
  this->setLimit(this->capacity());
  this->rewind();
}

void
ByteBuffer::flip()
{
  this->setLimit(this->position());
  this->rewind();
}

void
ByteBuffer::rewind()
{
  this->setPosition(0);
  this->discardMark();
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
