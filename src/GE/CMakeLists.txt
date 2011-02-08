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
#	@file rt^3/src/GE/CMakeLists.txt
#
#    Geometry Engine (ge)
#
#
##########################################################################

RT3_PROJECT(ge)

#Set Include Dirs
RT3_PROJECT_ADD_INCLUDE_DIRS(
)

#Set Libs
RT3_PROJECT_ADD_LIBS(
)

#set Source files
RT3_PROJECT_ADD_SOURCES (
	GeometryEngine.cxx
)

#Set INST Headers
RT3_PROJECT_ADD_INST_HEADERS(
)

#Set NOINST headers
RT3_PROJECT_ADD_NOINST_HEADERS(
	GeometryEngine.h
)

#Build the project
RT3_PROJECT_BUILD_LIB()
