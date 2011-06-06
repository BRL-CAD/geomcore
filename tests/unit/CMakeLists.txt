#########################################################################
#
#	BRL-CAD
#
#	Copyright (c) 1997-2011 United States Government as represented by
#	the U.S. Army Research Laboratory.
#
#	This library is free software; you can redistribute it and/or
#	modify it under the terms of the GNU Lesser General Public License
#	version 2.1 as published by the Free Software Foundation.
#
#	This library is distributed in the hope that it will be useful, but
#	WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#	Lesser General Public License for more details.
#
#	You should have received a copy of the GNU Lesser General Public
#	License along with this file; see the file named COPYING for more
#	information.
#
#########################################################################
#	@file geomcore/tests/unit/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR})

include_directories(
	${CPPUNIT_INCLUDE_DIR}
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

SET( UnitTests_LIBS
	${BRLCAD_BU_LIBRARY}
	${BRLCAD_PKG_LIBRARY}
	${TCL_LIBRARIES}
	${CPPUNIT_LIBRARY}
	libgcutil
	libNet
)

#FILE(GLOB_RECURSE UnitTests_SRCS *UTest.cxx )
SET( UnitTests_SRCS
	./utility/StringUtilsUTest.cxx
)
#	./utility/ByteBufferUTest.cxx

ADD_EXECUTABLE(runUnitTests test_runner.cxx ${UnitTests_SRCS})
TARGET_LINK_LIBRARIES(runUnitTests ${UnitTests_LIBS})

FOREACH(test ${UnitTests_SRCS})
#	MESSAGE(STATUS "Name:${test}")
    GET_FILENAME_COMPONENT(TestName ${test} NAME_WE)
    ADD_TEST(${TestName} runUnitTests ${TestName})
ENDFOREACH(test)
