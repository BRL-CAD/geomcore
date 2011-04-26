/*                   N E T M S G U T E S T . H
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
/** @file NetMsgUTest.h
 *
 * Brief description
 *
 */

#ifndef __TYPEONLYMSGUTEST_H__
#define __TYPEONLYMSGUTEST_H__

#include "TypeOnlyMsg.h"

#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>

class TypeOnlyMsgUTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( TypeOnlyMsgUTest );
  CPPUNIT_TEST( testNormCstrSerialize );
  CPPUNIT_TEST( testNormCstrSerializeBBProvided );
  CPPUNIT_TEST( testReplyCstrSerialize );
  CPPUNIT_TEST( testDeserialCstr );
  CPPUNIT_TEST( testGetters );
  CPPUNIT_TEST( testEquals );
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp(void);
  void tearDown(void);

protected:
  void testNormCstrSerialize();
  void testNormCstrSerializeBBProvided();
  void testReplyCstrSerialize();
  void testDeserialCstr();
  void testGetters();
  void testEquals();

  static char testData[];
};

#endif /* __TYPEONLYMSGUTEST_H__ */

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
