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
#	@file geomcore/src/libEvent/CMakeLists.txt
#
#    Event Library
#
##########################################################################

include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}
)

SET(libEvent_SRCS
    Event.cxx
    EventManager.cxx
    EventPublisher.cxx
    EventSubscriber.cxx
    EventSubscription.cxx
    DeliverEventJob.cxx
    SubmitEventJob.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libEvent SHARED ${libEvent_SRCS})
	target_link_libraries(libEvent libgcutil libJob)
	set_target_properties(libEvent PROPERTIES PREFIX "")
	install(TARGETS libEvent DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libEvent-static STATIC ${libEvent_SRCS})
	target_link_libraries(libEvent-static libgcutil libJob)
	IF(NOT WIN32)
		set_target_properties(libEvent-static PROPERTIES PREFIX "")
		set_target_properties(libEvent-static PROPERTIES OUTPUT_NAME "libEvent")
	ELSE(NOT WIN32)
		set_target_properties(libEvent-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libEvent-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

SET(libEvent_inst_HDRS
    Event.h
    EventTypes.h
    EventManager.h
    EventPublisher.h
    EventSubscriber.h
)
INSTALL(FILES ${libEvent_inst_HDRS} DESTINATION include)

