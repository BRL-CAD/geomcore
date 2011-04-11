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
 * Brief description
 *
 */

#ifndef __BYTEBUFFERUTEST_H__
#define __BYTEBUFFERUTEST_H__

#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>

class ByteBufferUTest : public CPPUNIT_NS::TestFixture
{
CPPUNIT_TEST_SUITE( ByteBufferUTest );
    CPPUNIT_TEST( testEmpty );
  CPPUNIT_TEST_SUITE_END();

public:
  void
  setUp(void);

  void
  tearDown(void);

protected:
  void
  testEmpty();
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
