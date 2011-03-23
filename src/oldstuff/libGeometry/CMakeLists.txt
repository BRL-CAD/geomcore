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
#	@file rt^3/src/GE/libGeometry/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

PROJECT(LibGeometry)

#Add additional include dirs to INCLUDE_DIRS as needed
SET(PROJ_INCLUDE_DIRS ${GLOBAL_INCLUDE_DIRS} )
INCLUDE_DIRECTORIES (${PROJ_INCLUDE_DIRS})

SET(PROJ_LINK_LIBS

)

#Print
INCLUDE (${CMAKE_MODULE_PATH}/ProjectPrinter.cmake)

#Set Sources
set(LIBGEOMETRY_SOURCES
	init.cxx 
	DXFFileParser.cxx 
	DXFFileParser.h 
	NFFFileParser.cxx 
	NFFFileParser.h 
	Point.cxx 
	Point.h 
	RGB.cxx 
	RGB.h 
	Scene.cxx 
	SceneFactory.cxx 
	Vec.cxx 
	Vec.h 
	View.cxx
)

ADD_LIBRARY (Geometry SHARED ${LIBGEOMETRY_SOURCES})
TARGET_LINK_LIBRARIES(Geometry ${PROJ_LINK_LIBS})

MESSAGE(STATUS "\tDone.")
