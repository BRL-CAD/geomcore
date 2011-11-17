#                      C M A K E L I S T S . T X T
#
#  Geometry Server
#  
#  Copyright (c) 1997-2011 United States Government as represented by
#  the U.S. Army Research Laboratory.
#  
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public License
#  version 2.1 as published by the Free Software Foundation.
#  
#  This library is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#  
#  You should have received a copy of the GNU Lesser General Public
#  License along with this file; see the file named COPYING for more
#  information.
#  

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
IF(COMMAND CMAKE_POLICY)
   CMAKE_POLICY(SET CMP0003 NEW)
   CMAKE_POLICY(SET CMP0007 OLD)
   IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} VERSION_GREATER 2.8.3)
      CMAKE_POLICY(SET CMP0017 OLD)
   ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} VERSION_GREATER 2.8.3)
ENDIF(COMMAND CMAKE_POLICY)

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
      SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
   ELSE(WIN32)
      SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
   ENDIF(WIN32)
ENDIF(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
IF(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
   SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all archives.")
ENDIF(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
IF(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
   SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
ENDIF(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)

FOREACH(CFG_TYPE ${CMAKE_CONFIGURATION_TYPES})
   STRING(TOUPPER "${CFG_TYPE}" CFG_TYPE)
   IF(NOT "CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}")
      IF(WIN32)
         SET("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}" ${CMAKE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
      ELSE(WIN32)
         SET("CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}" ${CMAKE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
      ENDIF(WIN32)
   ENDIF(NOT "CMAKE_LIBRARY_OUTPUT_DIRECTORY_${CFG_TYPE}")
   IF(NOT "CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}")
      SET("CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}" ${CMAKE_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all archives.")
   ENDIF(NOT "CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${CFG_TYPE}")
   IF(NOT "CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}")
      SET("CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}" ${CMAKE_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
   ENDIF(NOT "CMAKE_RUNTIME_OUTPUT_DIRECTORY_${CFG_TYPE}")
ENDFOREACH()


INCLUDE(TestBigEndian)
TEST_BIG_ENDIAN(WORDS_BIGENDIAN)
IF(WORDS_BIGENDIAN)
	add_definitions(-DWORDS_BIGENDIAN)
ENDIF(WORDS_BIGENDIAN)

ADD_SUBDIRECTORY(src/other)
ADD_SUBDIRECTORY(src)
