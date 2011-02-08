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
#	@file geomcore/src/libJob/CMakeLists.txt
#
#    Job Library
#
#
##########################################################################

include_directories(
   ${QT_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}
)

SET(libJob_SRCS 
	AbstractJob.cxx
	JobManager.cxx
	JobScheduler.cxx
	JobWorker.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libJob SHARED ${libJob_SRCS})
	target_link_libraries(libJob libgcutil ${QT_LIBRARIES})
	set_target_properties(libJob PROPERTIES PREFIX "")
	install(TARGETS libJob DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libJob-static STATIC ${libJob_SRCS})
	target_link_libraries(libJob-static libgcutil ${QT_LIBRARIES})
	IF(NOT WIN32)
		set_target_properties(libJob-static PROPERTIES PREFIX "")
		set_target_properties(libJob-static PROPERTIES OUTPUT_NAME "libJob")
	ELSE(NOT WIN32)
		set_target_properties(libJob-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libJob-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

SET(libJob_inst_HDRS
	AbstractJob.h
	JobManager.h
	JobScheduler.h
)
INSTALL(FILES ${libJob_inst_HDRS} DESTINATION include)
