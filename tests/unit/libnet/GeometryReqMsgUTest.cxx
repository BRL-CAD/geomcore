/*         G E O M E T R Y R E Q M S G U T E S T . C X X
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
/** @file GeometryReqMsgUTest.cxx
 *
 * Brief description
 *
 */

#include "GeometryReqMsg.h"

#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>

class GeometryReqMsgUTest : public CPPUNIT_NS::TestFixture {
  CPPUNIT_TEST_SUITE( GeometryReqMsgUTest );
  CPPUNIT_TEST( testNormCstrSerialize );
  CPPUNIT_TEST( testNormCstrSerializeBBProvided );
  CPPUNIT_TEST( testReplyCstrSerialize );
  CPPUNIT_TEST( testDeserialCstr );
  CPPUNIT_TEST( testGetters );
  CPPUNIT_TEST( testEquals );
  CPPUNIT_TEST_SUITE_END();

public:
  void
  setUp()
  {}

  void
  tearDown()
  {}

  void
  testNormCstrSerialize()
  {
    GeometryReqMsg msg(testPath, true);

    ByteBuffer* bb = msg.serialize();
    unsigned char* data = (unsigned char*) bb->array();

    /* type */
    CPPUNIT_ASSERT(data[0] == 0x04);
    CPPUNIT_ASSERT(data[1] == 0x00);

    GeometryReqMsgUTest::testCommonHeaderBytes(data);

    /* has recurse */
    CPPUNIT_ASSERT(data[79] == 0x01);

  delete bb;
  }

  void
  testNormCstrSerializeBBProvided()
  {
    GeometryReqMsg msg(testPath, true);

    ByteBuffer* bb = ByteBuffer::allocate(128);
    msg.serialize(bb);
    unsigned char* data = (unsigned char*) bb->array();

    /* type */
    CPPUNIT_ASSERT(data[0] == 0x04);
    CPPUNIT_ASSERT(data[1] == 0x00);

    GeometryReqMsgUTest::testCommonHeaderBytes(data);

     /* has recurse */
     CPPUNIT_ASSERT(data[79] == 0x01);

    delete bb;
  }

  void
  testReplyCstrSerialize()
  {
    GeometryReqMsg msg(testPath, true);

    GeometryReqMsg reMsg(&msg, testPath, true);

    ByteBuffer* bb = reMsg.serialize();
    unsigned char* data = (unsigned char*) bb->array();

    /* type */
    CPPUNIT_ASSERT(data[0] == 0x04);
    CPPUNIT_ASSERT(data[1] == 0x00);

    GeometryReqMsgUTest::testCommonHeaderBytes(data);

     /* has recurse */
     CPPUNIT_ASSERT(data[119] == 0x01);

    delete bb;
  }

  void
  testDeserialCstr()
  {
    ByteBuffer* bb = ByteBuffer::wrap(testData, 120);
    bb->flip();
    GeometryReqMsg msg(bb, NULL);

    ByteBuffer* bb2 = msg.serialize();
    char* data2 = bb2->array();

    for (int i = 0; i < 120; ++i){
        CPPUNIT_ASSERT(testData[i]==data2[i]);
    }

    delete bb;
    delete bb2;
  }

  void
  testGetters()
  {
    ByteBuffer* bb = ByteBuffer::wrap(testData, 120);
    bb->flip();
    GeometryReqMsg msg(bb, NULL);

    CPPUNIT_ASSERT(msg.getMsgType() == 0x400);
    CPPUNIT_ASSERT(msg.getMsgUUID()->toString() == "6c9a39d7-babb-469e-9a06-9c088159d096");
    CPPUNIT_ASSERT(msg.msgHasReUUID() == true);
    CPPUNIT_ASSERT(msg.getReUUID()->toString() == "4eef62e7-4cde-420c-873e-688cb1f34696");
    CPPUNIT_ASSERT(msg.getOrigin() == NULL);

    delete bb;
  }

  void
  testEquals()
  {
    ByteBuffer* bb1 = ByteBuffer::wrap(testData, 120);
    bb1->flip();
    GeometryReqMsg msg1(bb1, NULL);

    ByteBuffer* bb2 = ByteBuffer::wrap(testData, 120);
    bb2->flip();
    GeometryReqMsg msg2(bb2, NULL);

    CPPUNIT_ASSERT(msg1 == msg2);
    CPPUNIT_ASSERT(msg1.equals(msg2) == true);
    CPPUNIT_ASSERT(msg2.equals(msg1) == true);

    delete bb1;
    delete bb2;
  }

  static void testCommonHeaderBytes(unsigned char* data)
   {
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
     CPPUNIT_ASSERT((data[42] == 0x00) || (data[42] == 0x01));
     int offset = 0;

     if (data[42] == 0x01) {
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

       offset = 40;
     }

     /* Path String length */
     CPPUNIT_ASSERT(data[offset+43+0] == 0x00);
     CPPUNIT_ASSERT(data[offset+43+1] == 0x00);
     CPPUNIT_ASSERT(data[offset+43+2] == 0x00);
     CPPUNIT_ASSERT(data[offset+43+3] == 0x20);

     /* Path String */
     CPPUNIT_ASSERT(data[offset+47+0] == 0x61);
     CPPUNIT_ASSERT(data[offset+47+1] == 0x77);
     CPPUNIT_ASSERT(data[offset+47+2] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+3] == 0x73);
     CPPUNIT_ASSERT(data[offset+47+4] == 0x6f);
     CPPUNIT_ASSERT(data[offset+47+5] == 0x6d);
     CPPUNIT_ASSERT(data[offset+47+6] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+7] == 0x2f);
     CPPUNIT_ASSERT(data[offset+47+8] == 0x70);
     CPPUNIT_ASSERT(data[offset+47+9] == 0x61);
     CPPUNIT_ASSERT(data[offset+47+10] == 0x74);
     CPPUNIT_ASSERT(data[offset+47+11] == 0x68);
     CPPUNIT_ASSERT(data[offset+47+12] == 0x2f);
     CPPUNIT_ASSERT(data[offset+47+13] == 0x74);
     CPPUNIT_ASSERT(data[offset+47+14] == 0x6f);
     CPPUNIT_ASSERT(data[offset+47+15] == 0x2f);
     CPPUNIT_ASSERT(data[offset+47+16] == 0x61);
     CPPUNIT_ASSERT(data[offset+47+17] == 0x77);
     CPPUNIT_ASSERT(data[offset+47+18] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+19] == 0x73);
     CPPUNIT_ASSERT(data[offset+47+20] == 0x6f);
     CPPUNIT_ASSERT(data[offset+47+21] == 0x6d);
     CPPUNIT_ASSERT(data[offset+47+22] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+23] == 0x2f);
     CPPUNIT_ASSERT(data[offset+47+24] == 0x67);
     CPPUNIT_ASSERT(data[offset+47+25] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+26] == 0x6f);
     CPPUNIT_ASSERT(data[offset+47+27] == 0x6d);
     CPPUNIT_ASSERT(data[offset+47+28] == 0x65);
     CPPUNIT_ASSERT(data[offset+47+29] == 0x74);
     CPPUNIT_ASSERT(data[offset+47+30] == 0x72);
     CPPUNIT_ASSERT(data[offset+47+31] == 0x79);
   }

  static char testData[];
  static std::string testPath;
};

std::string GeometryReqMsgUTest::testPath = "awesome/path/to/awesome/geometry";
char GeometryReqMsgUTest::testData[] =
  {   0x04, 0x00, 0x00, 0x00, 0x00, 0x24, 0x36, 0x63, 0x39, 0x61, 0x33, 0x39,
      0x64, 0x37, 0x2d, 0x62, 0x61, 0x62, 0x62, 0x2d, 0x34, 0x36, 0x39, 0x65,
      0x2d, 0x39, 0x61, 0x30, 0x36, 0x2d, 0x39, 0x63, 0x30, 0x38, 0x38, 0x31,
      0x35, 0x39, 0x64, 0x30, 0x39, 0x36, 0x01, 0x00, 0x00, 0x00, 0x24, 0x34,
      0x65, 0x65, 0x66, 0x36, 0x32, 0x65, 0x37, 0x2d, 0x34, 0x63, 0x64, 0x65,
      0x2d, 0x34, 0x32, 0x30, 0x63, 0x2d, 0x38, 0x37, 0x33, 0x65, 0x2d, 0x36,
      0x38, 0x38, 0x63, 0x62, 0x31, 0x66, 0x33, 0x34, 0x36, 0x39, 0x36, 0x00,
      0x00, 0x00, 0x20, 0x61, 0x77, 0x65, 0x73, 0x6f, 0x6d, 0x65, 0x2f, 0x70,
      0x61, 0x74, 0x68, 0x2f, 0x74, 0x6f, 0x2f, 0x61, 0x77, 0x65, 0x73, 0x6f,
      0x6d, 0x65, 0x2f, 0x67, 0x65, 0x6f, 0x6d, 0x65, 0x74, 0x72, 0x79, 0x01 };

CPPUNIT_TEST_SUITE_REGISTRATION (GeometryReqMsgUTest);

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
