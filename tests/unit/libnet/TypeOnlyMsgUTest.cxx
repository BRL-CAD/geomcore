/*                 N E T M S G U T E S T . C X X
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
/** @file NetMsgUTest.cxx
 *
 * Brief description
 *
 */

#include "TypeOnlyMsg.h"

CPPUNIT_TEST_SUITE_REGISTRATION (TypeOnlyMsgUTest);

char TypeOnlyMsgUTest::testData[] = {
    0x09, 0xf7,
    0x00, 0x00,
    0x00, 0x24,
    0x30, 0x33,
    0x62, 0x64,
    0x30, 0x61,
    0x61, 0x61,
    0x2d,
    0x35, 0x39,
    0x63, 0x64,
    0x2d,
    0x34, 0x61,
    0x31, 0x35,
    0x2d,
    0x39, 0x63,
    0x63, 0x37,
    0x2d,
    0x62, 0x33,
    0x34, 0x30,
    0x37, 0x39,
    0x39, 0x35,
    0x31, 0x39,
    0x30, 0x35,
    0x01,
    0x00, 0x00,
    0x00, 0x24,
    0x30, 0x33,
    0x62, 0x64,
    0x30, 0x61,
    0x61, 0x61,
    0x2d,
    0x35, 0x39,
    0x63, 0x64,
    0x2d,
    0x34, 0x61,
    0x31, 0x35,
    0x2d,
    0x39, 0x63,
    0x63, 0x37,
    0x2d,
    0x62, 0x33,
    0x34, 0x30,
    0x37, 0x39,
    0x39, 0x35,
    0x31, 0x39,
    0x30, 0x35,
};

void
TypeOnlyMsgUTest::testNormCstrSerialize()
{
  TypeOnlyMsg msg(2551);

  ByteBuffer* bb = msg.serialize();
  unsigned char* data = (unsigned char*)bb->array();

  /* type */
  CPPUNIT_ASSERT(data[0] == 0x09);
  CPPUNIT_ASSERT(data[1] == 0xf7);

  /* Msg UUID LEN */
   CPPUNIT_ASSERT(data[2+0] == 0x00);
   CPPUNIT_ASSERT(data[2+1] == 0x00);
   CPPUNIT_ASSERT(data[2+2] == 0x00);
   CPPUNIT_ASSERT(data[2+3] == 0x24);

   /* Msg UUID */
   CPPUNIT_ASSERT(data[6+8] == '-');
   CPPUNIT_ASSERT(data[6+13] == '-');
   CPPUNIT_ASSERT(data[6+18] == '-');
   CPPUNIT_ASSERT(data[6+23] == '-');

   /* has RE:UUID */
   CPPUNIT_ASSERT(data[42] == 0x00);

  delete bb;
}

void
TypeOnlyMsgUTest::testNormCstrSerializeBBProvided()
{
  TypeOnlyMsg msg(2551);

  ByteBuffer* bb = ByteBuffer::allocate(128);
  msg.serialize(bb);
  unsigned char* data = (unsigned char*)bb->array();

  /* type */
  CPPUNIT_ASSERT(data[0] == 0x09);
  CPPUNIT_ASSERT(data[1] == 0xf7);

  /* Msg UUID LEN */
   CPPUNIT_ASSERT(data[2+0] == 0x00);
   CPPUNIT_ASSERT(data[2+1] == 0x00);
   CPPUNIT_ASSERT(data[2+2] == 0x00);
   CPPUNIT_ASSERT(data[2+3] == 0x24);

   /* Msg UUID */
   CPPUNIT_ASSERT(data[6+8] == '-');
   CPPUNIT_ASSERT(data[6+13] == '-');
   CPPUNIT_ASSERT(data[6+18] == '-');
   CPPUNIT_ASSERT(data[6+23] == '-');

   /* has RE:UUID */
   CPPUNIT_ASSERT(data[42] == 0x00);

  delete bb;
}

void
TypeOnlyMsgUTest::testReplyCstrSerialize()
{
  TypeOnlyMsg msg(1977);

  TypeOnlyMsg reMsg(2551, &msg);

  ByteBuffer* bb = reMsg.serialize();
  unsigned char* data = (unsigned char*)bb->array();

  /* type */
  CPPUNIT_ASSERT(data[0] == 0x09);
  CPPUNIT_ASSERT(data[1] == 0xf7);

  /* Msg UUID LEN */
  CPPUNIT_ASSERT(data[2+0] == 0x00);
  CPPUNIT_ASSERT(data[2+1] == 0x00);
  CPPUNIT_ASSERT(data[2+2] == 0x00);
  CPPUNIT_ASSERT(data[2+3] == 0x24);

  /* Msg UUID */
  CPPUNIT_ASSERT(data[6+8] == '-');
  CPPUNIT_ASSERT(data[6+13] == '-');
  CPPUNIT_ASSERT(data[6+18] == '-');
  CPPUNIT_ASSERT(data[6+23] == '-');

  /* has RE:UUID */
  CPPUNIT_ASSERT(data[42] == 0x01);

  /* Msg RE:UUID LEN */
  CPPUNIT_ASSERT(data[43+0] == 0x00);
  CPPUNIT_ASSERT(data[43+1] == 0x00);
  CPPUNIT_ASSERT(data[43+2] == 0x00);
  CPPUNIT_ASSERT(data[43+3] == 0x24);

  /* Msg RE:UUID */
  CPPUNIT_ASSERT(data[47+8] == '-');
  CPPUNIT_ASSERT(data[47+13] == '-');
  CPPUNIT_ASSERT(data[47+18] == '-');
  CPPUNIT_ASSERT(data[47+23] == '-');

  delete bb;
}

void
TypeOnlyMsgUTest::testDeserialCstr()
{
  ByteBuffer* bb = ByteBuffer::wrap(testData, 83);
  bb->flip();
  TypeOnlyMsg msg(bb, NULL);

  ByteBuffer* bb2 = msg.serialize();
  char* data2 = bb2->array();

  for (int i = 0; i < bb->position(); ++i) {
      CPPUNIT_ASSERT(testData[i]==data2[i]);
  }

  delete bb;
  delete bb2;
}

void
TypeOnlyMsgUTest::testGetters()
{
  ByteBuffer* bb = ByteBuffer::wrap(testData, 83);
  bb->flip();
  TypeOnlyMsg msg(bb, NULL);

  CPPUNIT_ASSERT(msg.getMsgType() == 2551);
  CPPUNIT_ASSERT(msg.getMsgUUID()->toString() == "03bd0aaa-59cd-4a15-9cc7-b34079951905");
  CPPUNIT_ASSERT(msg.msgHasReUUID() == true);
  CPPUNIT_ASSERT(msg.getReUUID()->toString() == "03bd0aaa-59cd-4a15-9cc7-b34079951905");
  CPPUNIT_ASSERT(msg.getOrigin() == NULL);

  delete bb;
}


void
TypeOnlyMsgUTest::testEquals()
{
  ByteBuffer* bb1 = ByteBuffer::wrap(testData, 83);
  bb1->flip();
  TypeOnlyMsg msg1(bb1, NULL);

  ByteBuffer* bb2 = ByteBuffer::wrap(testData, 83);
  bb2->flip();
  TypeOnlyMsg msg2(bb2, NULL);

  CPPUNIT_ASSERT(msg1 == msg2);
  CPPUNIT_ASSERT(msg1.equals(msg2) == true);
  CPPUNIT_ASSERT(msg2.equals(msg1) == true);

  delete bb1;
  delete bb2;
}

void
TypeOnlyMsgUTest::setUp()
{}

void
TypeOnlyMsgUTest::tearDown()
{}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
