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

  return new ByteBuffer(size);
}

ByteBuffer*
ByteBuffer::wrap(char* data, size_t size)
{
  if (size < 1)
    return NULL;

  if (data == NULL)
    data = (char*) bu_malloc(size, "BB.get()");

  ByteBuffer* bb = ByteBuffer::allocate(size);

  memcpy(bb->vlb.buf, data, size);
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
  if (this->position() + length > this->limit())
    return false;

  bu_vlb_write(&vlb, (unsigned char*) c, length);
  this->setPosition(this->position() + length);
  return true;
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

  char* temp = (char*) malloc(rem);
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

uint16_t
ByteBuffer::get16bit()
{
  // TODO IMPLEMENT ME
}

void
ByteBuffer::put16bit(uint16_t v)
{
  // TODO IMPLEMENT ME
}

uint32_t
ByteBuffer::get32bit()
{
  // TODO IMPLEMENT ME
}

void
ByteBuffer::put32bit(uint32_t v)
{
  // TODO IMPLEMENT ME
}

uint64_t
ByteBuffer::get64bit()
{
  // TODO IMPLEMENT ME
}

void
ByteBuffer::put64bit(uint64_t v)
{
  // TODO IMPLEMENT ME
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
ByteBuffer::setMark(size_t m)
{
  if (m < -1)
    return false;

  if (m > this->limit())
    return false;

  this->mar = m;
  return true;
}

size_t
ByteBuffer::getMark()
{
  return this->mar;
}

size_t
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
  size_t m = this->getMark();
  if (m < 0)
    return false;

  if (m > this->limit())
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
