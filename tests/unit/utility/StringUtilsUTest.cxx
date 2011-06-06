/*            S T R I N G U T I L S U T E S T . C X X
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
/** @file StringUtilsUTest.cxx
 *
 * Unit test designed to test the ByteBuffer class.
 *
 */

#include "StringUtils.h"

#include <cppunit/TestCase.h>
#include <cppunit/extensions/HelperMacros.h>
#include <string.h>

#include "bu.h"


class StringUtilsUTest : public CPPUNIT_NS::TestFixture
{
  CPPUNIT_TEST_SUITE( StringUtilsUTest );
  CPPUNIT_TEST( testGetLastStepOfPath );
  CPPUNIT_TEST( testSplitPathAtStep01 );
  CPPUNIT_TEST( testSplitPathAtStep01a );
  CPPUNIT_TEST( testSplitPathAtStep01b );
  CPPUNIT_TEST( testSplitPathAtStep01c );
  CPPUNIT_TEST( testSplitPathAtStep01d );
  CPPUNIT_TEST( testSplitPathAtStep01e );
  CPPUNIT_TEST( testSplitPathAtStep01f );
  CPPUNIT_TEST( testSplitPathAtStep01g );
  CPPUNIT_TEST( testSplitPathAtStep02 );
  CPPUNIT_TEST( testSplitPathAtStep03 );
  CPPUNIT_TEST( testSplitPathAtStep04 );
  CPPUNIT_TEST( testSplitPathAtStep05 );
  CPPUNIT_TEST( testSplitPathAtStep06 );
  CPPUNIT_TEST( testSplitPathAtStep07 );
  CPPUNIT_TEST( testSplitPathAtStep08 );
  CPPUNIT_TEST_SUITE_END();

public:

  void
  testGetLastStepOfPath()
  {
    std::string name;

    name =  StringUtils::basename(testPath01);
    CPPUNIT_ASSERT(name == "duder");
    name =  StringUtils::basename(testPath02);
    CPPUNIT_ASSERT(name == "duder");
    name =  StringUtils::basename(testPath03);
    CPPUNIT_ASSERT(name == "duder");
    name =  StringUtils::basename(testPath04);
    CPPUNIT_ASSERT(name == "duder");
  }


  void
  testSplitPathAtStep01()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 3, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path");
    CPPUNIT_ASSERT(rPath == "dude/or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }

  void
  testSplitPathAtStep01a()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 7, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path/dude/or/duder");
    CPPUNIT_ASSERT(rPath == "");
    CPPUNIT_ASSERT(totalSteps == 6);
  }


  void
  testSplitPathAtStep01b()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 6, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path/dude/or/duder");
    CPPUNIT_ASSERT(rPath == "");
    CPPUNIT_ASSERT(totalSteps == 6);
  }


  void
  testSplitPathAtStep01c()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 5, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path/dude/or");
    CPPUNIT_ASSERT(rPath == "duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }


  void
  testSplitPathAtStep01d()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 4, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path/dude");
    CPPUNIT_ASSERT(rPath == "or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }


  void
  testSplitPathAtStep01e()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 2, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some");
    CPPUNIT_ASSERT(rPath == "path/dude/or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }
void
  testSplitPathAtStep01f()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 1, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe");
    CPPUNIT_ASSERT(rPath == "some/path/dude/or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }
  void
  testSplitPathAtStep01g()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath01, 0, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/");
    CPPUNIT_ASSERT(rPath == "awe/some/path/dude/or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }



  void
  testSplitPathAtStep02()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath02, 3, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "/awe/some/path");
    CPPUNIT_ASSERT(rPath == "dude/or/duder/");
    CPPUNIT_ASSERT(totalSteps == 6);
  }

  void
  testSplitPathAtStep03()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath03, 3, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "awe/some/path");
    CPPUNIT_ASSERT(rPath == "dude/or/duder/");
    CPPUNIT_ASSERT(totalSteps == 6);
  }

  void
  testSplitPathAtStep04()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath04, 3, &lPath, &rPath, &totalSteps);
    CPPUNIT_ASSERT(lPath == "awe/some/path");
    CPPUNIT_ASSERT(rPath == "dude/or/duder");
    CPPUNIT_ASSERT(totalSteps == 6);
  }

  void
  testSplitPathAtStep05()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath05, 2, &lPath, &rPath, &totalSteps);

    CPPUNIT_ASSERT(lPath == "/awe/some");
    CPPUNIT_ASSERT(rPath == "path");
    CPPUNIT_ASSERT(totalSteps == 3);
  }
  void
  testSplitPathAtStep06()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath06, 2, &lPath, &rPath, &totalSteps);

    CPPUNIT_ASSERT(lPath == "/awe/some");
    CPPUNIT_ASSERT(rPath == "path/");
    CPPUNIT_ASSERT(totalSteps == 3);
  }
  void
  testSplitPathAtStep07()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath07, 2, &lPath, &rPath, &totalSteps);

    CPPUNIT_ASSERT(lPath == "awe/some");
    CPPUNIT_ASSERT(rPath == "path/");
    CPPUNIT_ASSERT(totalSteps == 3);
  }
  void
  testSplitPathAtStep08()
  {
    std::string rPath = "";
    std::string lPath = "";
    int totalSteps = 0;

    StringUtils::splitPathAtStep(testPath08, 2, &lPath, &rPath, &totalSteps);

    CPPUNIT_ASSERT(lPath == "awe/some");
    CPPUNIT_ASSERT(rPath == "path");
    CPPUNIT_ASSERT(totalSteps == 3);
  }


  void
  setUp()
  {

  }

  void
  tearDown()
  {

  }

private:
  static const std::string testPath01;
  static const std::string testPath02;
  static const std::string testPath03;
  static const std::string testPath04;
  static const std::string testPath05;
  static const std::string testPath06;
  static const std::string testPath07;
  static const std::string testPath08;
};

const std::string StringUtilsUTest::testPath01 = "/awe/some/path/dude/or/duder";
const std::string StringUtilsUTest::testPath02 = "/awe/some/path/dude/or/duder/";
const std::string StringUtilsUTest::testPath03 = "awe/some/path/dude/or/duder/";
const std::string StringUtilsUTest::testPath04 = "awe/some/path/dude/or/duder";
const std::string StringUtilsUTest::testPath05 = "///awe///some///path";
const std::string StringUtilsUTest::testPath06 = "///awe///some///path///";
const std::string StringUtilsUTest::testPath07 = "awe///some///path///";
const std::string StringUtilsUTest::testPath08 = "awe///some///path";

CPPUNIT_TEST_SUITE_REGISTRATION (StringUtilsUTest);

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
