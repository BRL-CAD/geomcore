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
#	@file geomcore/tests/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/../src/other/uuid
	${CMAKE_CURRENT_BINARY_DIR}/../src/other/uuid
)

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${GEOMCORE_BINARY_DIR}/tests-bin CACHE INTERNAL "Single output directory for building all test executables.")

ADD_SUBDIRECTORY(libNet)
ADD_SUBDIRECTORY(GE)
ADD_SUBDIRECTORY(GS)
ADD_SUBDIRECTORY(utility)
ADD_SUBDIRECTORY(libJob)
ADD_SUBDIRECTORY(libpkgcpp)
ADD_SUBDIRECTORY(libEvent)
IF(BUILD_SVN_TARGETS)
	GET_DIRECTORY_PROPERTY(APR_INCLUDE_DIR DIRECTORY ../src/other/subversion DEFINITION APR_INCLUDE_DIR)
	GET_DIRECTORY_PROPERTY(APU_INCLUDE_DIR DIRECTORY ../src/other/subversion DEFINITION APU_INCLUDE_DIR)
	include_directories(
		${APR_INCLUDE_DIR}
		${APU_INCLUDE_DIR}
		${CMAKE_SOURCE_DIR}/src/other/subversion/include
		${CMAKE_SOURCE_DIR}/src/other/subversion/include/private
		${CMAKE_BINARY_DIR}/src/other/subversion/include
		)
	ADD_SUBDIRECTORY(svntest)
ENDIF(BUILD_SVN_TARGETS)
#ADD_SUBDIRECTORY(coreInterface)
