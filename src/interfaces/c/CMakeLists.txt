#########################################################################
#
#	BRL-CAD
#
#	Copyright (c) 1997-2010 United States Government as represented by
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
#	@file rt^3/src/clients/c/CMakeLists.txt
#
##########################################################################

PROJECT(gsclient)
add_library(gsclient SHARED gsclient.c)
add_library(gsclient-static STATIC gsclient.c)
IF(NOT WIN32)
	SET_TARGET_PROPERTIES(gsclient-static PROPERTIES OUTPUT_NAME "gsclient")
ENDIF(NOT WIN32)

