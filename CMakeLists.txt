#                      C M A K E L I S T S . T X T
#
#	BRL-CAD - Geometry Core
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

CMAKE_MINIMUM_REQUIRED(VERSION 2.2)
IF(COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
	CMAKE_POLICY(SET CMP0007 OLD)
ENDIF(COMMAND CMAKE_POLICY)

PROJECT(GEOMCORE)

#Set path for cmake modules.
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

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

#---------------------------------------------------------------------
# Output directories - this is where built library and executable 
# files will be placed after building but prior to install.
IF(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
	IF(WIN32)
		SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
	ELSE(WIN32)
		SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
	ENDIF(WIN32)
ENDIF(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
IF(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
	SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all archives.")
ENDIF(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
IF(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
	SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
ENDIF(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)

FOREACH(CFG_TYPE ${CMAKE_CONFIGURATION_TYPES})
	STRING(TOUPPER "${CFG_TYPE}" CFG_TYPE)
	IF(NOT "CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}")
		IF(WIN32)
			SET("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}" ${GEOMCORE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
		ELSE(WIN32)
			SET("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}" ${GEOMCORE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
		ENDIF(WIN32)
	ENDIF(NOT "CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}")
	IF(NOT "CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}")
		SET("CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}" ${GEOMCORE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all archives.")
	ENDIF(NOT "CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}")
	IF(NOT "CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}")
		SET("CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}" ${GEOMCORE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
	ENDIF(NOT "CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}")
ENDFOREACH()

# Build shared libs by default.  Mark this as advanced - turning off
# ALL shared library building is unlikely to result in a working build
# and is not a typical configuration.  Note that turning this option off
# will not disable libraries specifically added as SHARED.
OPTION(BUILD_SHARED_LIBS "Build shared libraries" ON)
MARK_AS_ADVANCED(BUILD_SHARED_LIBS)

# Build static libs by default.  Standard practice is to build and
# install both dynamic and static versions of libraries.  This option
# is off by default in Debug configuration.  Note that turning this
# option off will not disable libraries specifically added as STATIC
IF(CMAKE_BUILD_TYPE)
	# If we're doing a debug build, don't build the static libs unless
	# they're specifically asked for (i.e. there is a situation where
	# the developer wants to look into the static libs specifically.
	# Note that this won't turn off libs that are built ONLY as static
	IF ("${CMAKE_BUILD_TYPE}" MATCHES "Debug" AND NOT MSVC)
		OPTION(BUILD_STATIC_LIBS "Build static libraries" OFF)
	ENDIF ("${CMAKE_BUILD_TYPE}" MATCHES "Debug" AND NOT MSVC)
ELSE(CMAKE_BUILD_TYPE)
	OPTION(BUILD_STATIC_LIBS "Build static libraries" ON)
ENDIF(CMAKE_BUILD_TYPE)

# We need BRL-CAD
find_package(BRLCAD REQUIRED)

# If BRL-CAD doesn't provide TCL, we need to find it
find_package(TCL REQUIRED)

# For the moment, we need Qt
find_package(Qt4 REQUIRED QtCore QtNetwork)

# Configure QT4
ADD_DEFINITIONS(${QT_DEFINITIONS})
include(${QT_USE_FILE})
SET(QT_VERSION "${QT_VERSION_MAJOR}.${QT_VERSION_MINOR}.${QT_VERSION_PATCH}")

# Always include the toplevel include dir
include_directories(
	${GEOMCORE_SOURCE_DIR}/include
	)

# Global setup done, on to specific projects
ADD_SUBDIRECTORY(src)

# Set up Docs
#ADD_SUBDIRECTORY(docs)

OPTION(GEOMCORE_BUILD_TESTS "Build GeomCore Tests" ON)
#Decend into the test tree.
IF (GEOMCORE_BUILD_TESTS)
	ADD_SUBDIRECTORY(tests)
ENDIF (GEOMCORE_BUILD_TESTS)

