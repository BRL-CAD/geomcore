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
PROJECT(sqlite)

find_package(Threads REQUIRED)

include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}
	)

IF(BUILD_SHARED_LIBS)
	add_library(libsqlite3 SHARED sqlite3.c)
	target_link_libraries(libsqlite3 ${CMAKE_THREAD_LIBS_INIT})
	set_target_properties(libsqlite3 PROPERTIES PREFIX "")
	install(TARGETS libsqlite3 DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libsqlite3-static STATIC sqlite3.c)
	target_link_libraries(libsqlite3-static ${CMAKE_THREAD_LIBS_INIT})
	IF(NOT WIN32)
		set_target_properties(libsqlite3-static PROPERTIES PREFIX "")
		set_target_properties(libsqlite3-static PROPERTIES OUTPUT_NAME "${libname}")
	ELSE(NOT WIN32)
		set_target_properties(libsqlite3-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libsqlite3-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

add_executable(sqlite shell.c)
target_link_libraries(sqlite libsqlite3)
install(TARGETS sqlite DESTINATION bin)
