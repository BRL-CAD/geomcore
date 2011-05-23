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
#	@file rt^3/tests/utility/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

include_directories(
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

add_executable(gloggerTest loggerTest.cxx)
target_link_libraries(gloggerTest libgcutil)

add_executable(gthreadTest threadTest.cxx)
target_link_libraries(gthreadTest libgcutil)

add_executable(gconfigTest configTest.cxx)
target_link_libraries(gconfigTest libgcutil)

add_executable(guuidTest uuidTest.cxx)
target_link_libraries(guuidTest libgcutil)

file(COPY test.config DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
