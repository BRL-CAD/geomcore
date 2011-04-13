/*             B Y T E B U F F E R U T E S T . C X X
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
/** @file ByteBufferUTest.cxx
 *
 * Brief description
 *
 */

#include "ByteBufferUTest.h"

CPPUNIT_TEST_SUITE_REGISTRATION (ByteBufferUTest);

void
ByteBufferUTest::testAllocate()
{
  delete this->bb;
  this->bb = ByteBuffer::allocate(defaultSize);
  CPPUNIT_ASSERT( this->bb != NULL);
}

void
ByteBufferUTest::testCapacity()
{
  CPPUNIT_ASSERT( this->bb->capacity() == ByteBufferUTest::defaultSize);
}

void
ByteBufferUTest::testRemaining()
{
  CPPUNIT_ASSERT( this->bb->remaining() == ByteBufferUTest::defaultSize);
}

void
ByteBufferUTest::testGetMark()
{
  /* Check default */
  CPPUNIT_ASSERT(  this->bb->getMark() == -1 );
}

void
ByteBufferUTest::testMark()
{
  this->bb->mark();

  /* Check set to zero */
  CPPUNIT_ASSERT(  this->bb->getMark() == 0 );
}
void
ByteBufferUTest::testDiscardMark()
{
  this->bb->mark();
  this->bb->discardMark();

  /* Check back to default */
  CPPUNIT_ASSERT(  this->bb->getMark() == -1 );
}

void
ByteBufferUTest::setUp()
{
//  std::cout << "\nSetUp.\n";
  this->bb = ByteBuffer::allocate(defaultSize);
}

void
ByteBufferUTest::tearDown()
{
//  std::cout << "\nTearDown.\n";
  delete this->bb;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
