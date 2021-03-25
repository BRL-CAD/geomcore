#                      C M A K E L I S T S . T X T
#
# BRL-CAD - Geometry Service Test Client
#
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
#
# Note - if this test client develops into anything useful should
# probably BSD license it to make client building easier.

cmake_minimum_required(VERSION 3.12)

project(GSTESTCLIENT)

#Set path for cmake modules.
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake")

# We need BRL-CAD
find_package(BRLCAD REQUIRED)

# If BRL-CAD doesn't provide TCL, we need to find it
find_package(TCL REQUIRED)

# If there's a UUID library, we want it
#INCLUDE(CheckLibraryExists)
#CHECK_LIBRARY_EXISTS(uuid uuid_generate "" HAVE_UUID)
#IF(HAVE_UUID)
#	SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -luuid")
#	SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -luuid")
#	SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -luuid")
#ENDIF(HAVE_UUID)

# Add debugging flag for now
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")

# Set the include directories
include_directories(
  ${TCL_INCLUDE_DIRS}
  ${BRLCAD_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_BINARY_DIR}/src/other/uuid
  )

# And now, to business
add_executable(gstestclient gstestclient.c)
target_link_libraries(gstestclient ${BRLCAD_PKG_LIBRARY} ${BRLCAD_BU_LIBRARY} uuid)

# Local Variables:
# tab-width: 8
# mode: cmake
# indent-tabs-mode: t
# End:
# ex: shiftwidth=2 tabstop=8
