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
 * Unit test designed to test the ByteBuffer class.
 *
 */

#include "ByteBuffer.h"
#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>
#include <string.h>

class ByteBufferUTest : public CPPUNIT_NS::TestFixture {
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

  void
  testAllocate()
  {
    delete this->bb;
    this->bb = ByteBuffer::allocate(defaultSize);
    CPPUNIT_ASSERT( this->bb != NULL);
  }

  void
  testWrap()
  {
    /* taken from vlb.c */
    int VLB_BLOCK_SIZE = 512;

    delete this->bb;
    this->bb = ByteBuffer::wrap((char*)&testVal_64bit_net, 8);
    CPPUNIT_ASSERT( this->bb != NULL);
    CPPUNIT_ASSERT( this->bb->capacity() == 8 + VLB_BLOCK_SIZE);
    CPPUNIT_ASSERT( this->bb->limit() == 8);
    CPPUNIT_ASSERT( this->bb->position() == 8);
  }

  void
  testCapacity()
  {
    CPPUNIT_ASSERT( this->bb->capacity() == defaultSize);
  }

  void
  testRemaining()
  {
    CPPUNIT_ASSERT( this->bb->remaining() == defaultSize);
  }

  void
  testGetMark()
  {
    /* Check default */
    CPPUNIT_ASSERT(  this->bb->getMark() == -1 );
  }

  void
  testMark()
  {
    this->bb->mark();

    /* Check set to zero */
    CPPUNIT_ASSERT(  this->bb->getMark() == 0 );
  }

  void
  testDiscardMark()
  {
    this->bb->mark();
    this->bb->discardMark();

    /* Check back to default */
    CPPUNIT_ASSERT( this->bb->getMark() == -1 );
  }

  void
  testPut8bit()
  {
    this->bb->put(testVal_8bit);
    char* data = this->bb->array();

    CPPUNIT_ASSERT(data[0] == testVal_8bit);
    CPPUNIT_ASSERT(this->bb->position() == 1);
  }

  void
  testPut16bit()
  {
    this->bb->put16bit(testVal_16bit_host);
    char* data = this->bb->array();
    char* d = (char*)(&testVal_16bit_net);

    CPPUNIT_ASSERT(data[0] == d[0]);
    CPPUNIT_ASSERT(data[1] == d[1]);

    CPPUNIT_ASSERT(this->bb->position() == 2);
  }

  void
  testPut32bit()
  {
    this->bb->put32bit(testVal_32bit_host);
    char* data = this->bb->array();
    char* d = (char*)(&testVal_32bit_net);

    CPPUNIT_ASSERT(data[0] == d[0]);
    CPPUNIT_ASSERT(data[1] == d[1]);
    CPPUNIT_ASSERT(data[2] == d[2]);
    CPPUNIT_ASSERT(data[3] == d[3]);

    CPPUNIT_ASSERT(this->bb->position() == 4);
  }

  void
  testPut64bit()
  {
    this->bb->put64bit(testVal_64bit_host);
    char* data = this->bb->array();

    char* d = (char*)(&testVal_64bit_net);

    CPPUNIT_ASSERT(data[0] == d[0]);
    CPPUNIT_ASSERT(data[1] == d[1]);
    CPPUNIT_ASSERT(data[2] == d[2]);
    CPPUNIT_ASSERT(data[3] == d[3]);
    CPPUNIT_ASSERT(data[4] == d[4]);
    CPPUNIT_ASSERT(data[5] == d[5]);
    CPPUNIT_ASSERT(data[6] == d[6]);
    CPPUNIT_ASSERT(data[7] == d[7]);

    CPPUNIT_ASSERT(this->bb->position() == 8);
  }

  void
  testGet8bit()
  {
    char* data = this->bb->array();
    data[0] = testVal_8bit;
    uint8_t val = this->bb->get();

    CPPUNIT_ASSERT(val == testVal_8bit);
    CPPUNIT_ASSERT(this->bb->position() == 1);
  }

  void
  testGet16bit()
  {
    char* data = this->bb->array();
    char* d = (char*)(&testVal_16bit_net);

    data[0] = d[0];
    data[1] = d[1];

    uint16_t val = this->bb->get16bit();

    CPPUNIT_ASSERT(val == testVal_16bit_host);
    CPPUNIT_ASSERT(this->bb->position() == 2);
  }

  void
  testGet32bit()
  {
    char* data = this->bb->array();
    char* d = (char*)(&testVal_32bit_net);

    data[0] = d[0];
    data[1] = d[1];
    data[2] = d[2];
    data[3] = d[3];
    uint32_t val = this->bb->get32bit();

    CPPUNIT_ASSERT(val == testVal_32bit_host);
    CPPUNIT_ASSERT(this->bb->position() == 4);
  }

  void
  testGet64bit()
  {
    char* data = this->bb->array();
    char* d = (char*)(&testVal_64bit_net);

    data[0] = d[0];
    data[1] = d[1];
    data[2] = d[2];
    data[3] = d[3];
    data[4] = d[4];
    data[5] = d[5];
    data[6] = d[6];
    data[7] = d[7];
    uint64_t val = this->bb->get64bit();

    CPPUNIT_ASSERT(val == testVal_64bit_host);
    CPPUNIT_ASSERT(this->bb->position() == 8);
  }

  void
  testClear()
  {
    this->bb->put64bit(testVal_64bit_host);
    this->bb->put64bit(testVal_64bit_host);
    this->bb->put64bit(testVal_64bit_host);

    CPPUNIT_ASSERT(this->bb->position() == 24);

    this->bb->clear();

    CPPUNIT_ASSERT(this->bb->position() == 0);
    CPPUNIT_ASSERT(this->bb->limit() == this->bb->capacity());
    CPPUNIT_ASSERT(this->bb->getMark() == -1);
  }

  void
  testFlip()
  {
    this->bb->put64bit(testVal_64bit_host);
    this->bb->put64bit(testVal_64bit_host);
    this->bb->put64bit(testVal_64bit_host);

    CPPUNIT_ASSERT(this->bb->position() == 24);

    this->bb->flip();

    CPPUNIT_ASSERT(this->bb->position() == 0);
    CPPUNIT_ASSERT(this->bb->limit() == 24);
    CPPUNIT_ASSERT(this->bb->getMark() == -1);
  }

  void
  testCompact()
  {
    this->bb->put64bit(0x0000000000000001);
    this->bb->put64bit(0x0000000000000002);
    this->bb->put64bit(0x0000000000000003);
    this->bb->flip();
    uint64_t val = this->bb->get64bit();

    CPPUNIT_ASSERT(val == 0x0000000000000001);

    CPPUNIT_ASSERT(this->bb->position() == 8);
    CPPUNIT_ASSERT(this->bb->limit() == 24);
    CPPUNIT_ASSERT(this->bb->getMark() == -1);

    this->bb->compact();

    CPPUNIT_ASSERT(this->bb->position() == 16);
    CPPUNIT_ASSERT(this->bb->limit() == this->bb->capacity());
    CPPUNIT_ASSERT(this->bb->getMark() == -1);
  }

  void
  testDuplicate()
  {
    this->bb->put64bit(0x0000000000000001);
    this->bb->mark();
    this->bb->put64bit(0x0000000000000002);
    this->bb->put64bit(0x0000000000000003);

    ByteBuffer* bbtwo = this->bb->duplicate();

    CPPUNIT_ASSERT(this->bb->position() == 24);
    CPPUNIT_ASSERT(this->bb->limit() == this->bb->capacity());
    CPPUNIT_ASSERT(this->bb->getMark() == 8);

    CPPUNIT_ASSERT(bbtwo->position() == 24);
    CPPUNIT_ASSERT(bbtwo->limit() == bbtwo->capacity());
    CPPUNIT_ASSERT(bbtwo->getMark() == 8);

    delete bbtwo;
  }

  void
  testLimit()
  {
    this->bb->setLimit(32);
    CPPUNIT_ASSERT(this->bb->limit() == 32);

    this->bb->setPosition(0);
    this->bb->put64bit(0x00); // 08
    this->bb->put64bit(0x00); // 16
    this->bb->put64bit(0x00); // 24
    this->bb->put64bit(0x00); // 32
    this->bb->put64bit(0x00); // 40
    this->bb->put64bit(0x00); // 48
    this->bb->put64bit(0x00); // 56
    this->bb->put64bit(0x00); // 64
    this->bb->put64bit(0x00); // 72
    this->bb->put64bit(0x00); // 80
    this->bb->put64bit(0x00); // 88
    this->bb->put64bit(0x00); // 96

    CPPUNIT_ASSERT(this->bb->limit() == 96);
  }

  void
  testPosition()
  {
    this->bb->put64bit(0x0000000000000001);
    this->bb->put64bit(0x0000000000000002);
    this->bb->put64bit(0x0000000000000003);

    CPPUNIT_ASSERT(this->bb->position() == 24);
  }

  void
  testReset()
  {
    this->bb->put64bit(0x0000000000000001);
    this->bb->mark();
    this->bb->put64bit(0x0000000000000002);
    this->bb->put64bit(0x0000000000000003);

    CPPUNIT_ASSERT(this->bb->position() == 24);
    this->bb->reset();
    CPPUNIT_ASSERT(this->bb->position() == 8);
  }

  void
  testRewind()
  {
    this->testPosition();
    this->bb->rewind();

    CPPUNIT_ASSERT(this->bb->position() == 0);
  }

  void
  testSetPosition()
  {
    this->bb->setPosition(24);

    CPPUNIT_ASSERT(this->bb->position() == 24);
  }

  void
  testGetSetString()
  {
    this->bb->putString(testVal_StdString);
    this->bb->flip();
    std::string result = this->bb->getString();
    CPPUNIT_ASSERT(result == testVal_StdString);
  }

  void
  setUp()
  {
    this->bb = ByteBuffer::allocate(defaultSize);
  }

  void
  tearDown()
  {
    delete this->bb;
  }

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

const size_t ByteBufferUTest::defaultSize = 1024;
const uint8_t ByteBufferUTest::testVal_8bit = 0x2A;
const uint16_t ByteBufferUTest::testVal_16bit_host = 0x2A3B;
const uint16_t ByteBufferUTest::testVal_16bit_net = 0x3B2A;
const uint32_t ByteBufferUTest::testVal_32bit_host = 0x2A3B4C5D;
const uint32_t ByteBufferUTest::testVal_32bit_net = 0x5D4C3B2A;
const uint64_t ByteBufferUTest::testVal_64bit_host = 0x2A3B4C5D6E7F8A9B;
const uint64_t ByteBufferUTest::testVal_64bit_net = 0x9B8A7F6E5D4C3B2A;
const std::string ByteBufferUTest::testVal_StdString = "The quick brown fox jumps over the lazy dog";

CPPUNIT_TEST_SUITE_REGISTRATION (ByteBufferUTest);

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
