/*             B Y T E B U F F E R U T E S T . H
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
/** @file ByteBufferUTest.h
 *
 * Unit test designed to test the ByteBuffer class.
 *
 */

#ifndef __BYTEBUFFERUTEST_H__
#define __BYTEBUFFERUTEST_H__

#include "ByteBuffer.h"
#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>
#include <string.h>

class ByteBufferUTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( ByteBufferUTest );
  CPPUNIT_TEST( testAllocate );
  CPPUNIT_TEST( testWrap );
  CPPUNIT_TEST( testCapacity );
  CPPUNIT_TEST( testRemaining );
  CPPUNIT_TEST( testGetMark );
  CPPUNIT_TEST( testMark );
  CPPUNIT_TEST( testDiscardMark );
  CPPUNIT_TEST( testPut8bit );
  CPPUNIT_TEST( testPut16bit );
  CPPUNIT_TEST( testPut32bit );
  CPPUNIT_TEST( testPut64bit );

  CPPUNIT_TEST( testGet8bit );
  CPPUNIT_TEST( testGet16bit );
  CPPUNIT_TEST( testGet32bit );
  CPPUNIT_TEST( testGet64bit );

  CPPUNIT_TEST( testClear );
  CPPUNIT_TEST( testFlip );
  CPPUNIT_TEST( testCompact );
  CPPUNIT_TEST( testDuplicate );
  CPPUNIT_TEST( testLimit );
  CPPUNIT_TEST( testPosition );
  CPPUNIT_TEST( testSetPosition );

  CPPUNIT_TEST( testReset );
  CPPUNIT_TEST( testRewind );
  CPPUNIT_TEST( testGetSetString );

  CPPUNIT_TEST_SUITE_END();

public:
  void setUp(void);
  void tearDown(void);

protected:
  void testAllocate();
  void testWrap();
  void testCapacity();
  void testRemaining();
  void testGetMark();
  void testMark();
  void testDiscardMark();

  void testPut8bit();
  void testPut16bit();
  void testPut32bit();
  void testPut64bit();

  void testGet8bit();
  void testGet16bit();
  void testGet32bit();
  void testGet64bit();

  void testClear();
  void testFlip();
  void testCompact();
  void testDuplicate();

  void testLimit();
  void testPosition();
  void testSetPosition();
  void testReset();
  void testRewind();
  void testGetSetString();

private:
  ByteBuffer* bb;

  static const size_t defaultSize;
  static const uint8_t testVal_8bit;
  static const uint16_t testVal_16bit_host;
  static const uint16_t testVal_16bit_net;
  static const uint32_t testVal_32bit_host;
  static const uint32_t testVal_32bit_net;
  static const uint64_t testVal_64bit_host;
  static const uint64_t testVal_64bit_net;
  static const std::string testVal_StdString;
};

#endif // __BYTEBUFFERUTEST_H__

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
