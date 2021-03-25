#               C M A K E L I S T S . T X T
#
# BRL-CAD - Geometry Core

# Copyright (c) 1997-2011 United States Government as represented by
# the U.S. Army Research Laboratory.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this file; see the file named COPYING for more
# information.

cmake_minimum_required(VERSION 3.12)

project(GEOMCORE)

#Set path for cmake modules.
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake")

include(Path_Setup)

# Enable/disable Subversion related build (default off)
option(BUILD_SVN_TARGETS "Build subversion related code" OFF)

# Enable the doxygen documentation build
option(BUILD_GEOMCORE_DOXYGEN_DOCS "Create doxygen documentation for developers" OFF)

# There is a test client for the Geometry Service in src/GS/testclient
# that does not and should not share headers or other code with
# the rest of geomcore - the idea is that it should have knowledge
# only of the GS protocal.  Consequently, we configure that directory
# before setting any include directories or compile flags for the
# rest of geomcore.
add_subdirectory(src/GS/testclient)

# We need BRL-CAD
find_package(BRLCAD REQUIRED)

# If BRL-CAD doesn't provide TCL, we need to find it
find_package(TCL REQUIRED)

# Always include the toplevel include dir
include_directories(
  ${GEOMCORE_SOURCE_DIR}/include
  )

# Add debugging flag for now
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")

# Global setup done, on to specific projects
add_subdirectory(src)

#Descend into the test tree.

option(GEOMCORE_BUILD_FUNC_TESTS "Build GeomCore Functional Tests" ON)
if (GEOMCORE_BUILD_FUNC_TESTS)
  add_subdirectory(tests/func)
endif (GEOMCORE_BUILD_FUNC_TESTS)

option(GEOMCORE_BUILD_UNIT_TESTS "Build GeomCore Unit Tests" ON)
if (GEOMCORE_BUILD_UNIT_TESTS)
  #CppUnit is required for unit tests.
  find_package(CPPUNIT REQUIRED)

  if (CPPUNIT_FOUND)
    add_subdirectory(tests/unit)
  else (CPPUNIT_FOUND)
    message(STATUS "CppUnit not found, cannot configure unit tests.")
  endif (CPPUNIT_FOUND)
endif (GEOMCORE_BUILD_UNIT_TESTS)

# Local Variables:
# tab-width: 8
# mode: cmake
# indent-tabs-mode: t
# End:
# ex: shiftwidth=2 tabstop=8
