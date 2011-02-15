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


ADD_SUBDIRECTORY(libNet)
ADD_SUBDIRECTORY(GE)
ADD_SUBDIRECTORY(GS)
ADD_SUBDIRECTORY(utility)
ADD_SUBDIRECTORY(libJob)
ADD_SUBDIRECTORY(libpkgcpp)
ADD_SUBDIRECTORY(libEvent)
#ADD_SUBDIRECTORY(coreInterface)
