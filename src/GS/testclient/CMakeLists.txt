#                      C M A K E L I S T S . T X T
#
#	BRL-CAD - Geometry Service Test Client
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

#  Note - if this test client develops into anything useful should
#  probably BSD license it to make client building easier.

CMAKE_MINIMUM_REQUIRED(VERSION 2.2)
IF(COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
	CMAKE_POLICY(SET CMP0007 OLD)
ENDIF(COMMAND CMAKE_POLICY)

PROJECT(GSTESTCLIENT)

#Set path for cmake modules.
SET(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake")

#---------------------------------------------------------------------
# We want to check /usr/local by default, so add it 
SET(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} /usr/local)
SET(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} /usr/local/include)

#---------------------------------------------------------------------
# The location in which to install BRLCAD. Need a good Debug location 
# for Windows.  Only do this if CMAKE_INSTALL_PREFIX hasn't been set
# already, to try and allow parent builds (if any) some control.
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
	IF(NOT WIN32)
		IF ("${CMAKE_BUILD_TYPE}" MATCHES "Debug")
			SET(CMAKE_INSTALL_PREFIX "${GEOMCORE_SOURCE_DIR}/../geomcore-install")
		ELSE("${CMAKE_BUILD_TYPE}" MATCHES "Debug")
			SET(CMAKE_INSTALL_PREFIX "/usr/brlcad")
		ENDIF ("${CMAKE_BUILD_TYPE}" MATCHES "Debug")
	ENDIF(NOT WIN32)
	SET(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE STRING "GEOMCORE install prefix" FORCE)
	SET(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT 0)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
SET(GEOMCORE_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE STRING "GEOMCORE install prefix")

#---------------------------------------------------------------------
# The following logic is what allows binaries to run successfully in 
# the build directory AND install directory.  Thanks to plplot for 
# identifying the necessity of setting CMAKE_INSTALL_NAME_DIR on OSX.

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

# the RPATH/INSTALL_NAME_DIR to be used when installing
if (NOT APPLE)
	SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib:\$ORIGIN/../lib")
endif(NOT APPLE)
SET(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH which point to 
# directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# We need BRL-CAD
find_package(BRLCAD REQUIRED)

# If BRL-CAD doesn't provide TCL, we need to find it
find_package(TCL REQUIRED)

# If there's a UUID library, we want it
INCLUDE(CheckLibraryExists)
CHECK_LIBRARY_EXISTS(uuid uuid_generate "" HAVE_UUID)
IF(HAVE_UUID)
	SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -luuid")
	SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -luuid")
	SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -luuid")
ENDIF(HAVE_UUID)

# Add debugging flag for now
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")

# Set the include directories
include_directories(
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
)

# And now, to business
add_executable(gstestclient gstestclient.c tpl.c)
target_link_libraries(gstestclient ${BRLCAD_PKG_LIBRARY} ${BRLCAD_BU_LIBRARY})
