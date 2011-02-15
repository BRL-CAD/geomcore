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
#	@file geomcore/src/utility/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

include_directories(
    ${QT_INCLUDE_DIR}
    ${TCL_INCLUDE_DIRS}
	 ${BRLCAD_INCLUDE_DIRS}
	 ${CMAKE_CURRENT_SOURCE_DIR}/../other/uuid
	 ${CMAKE_CURRENT_BINARY_DIR}/../other/uuid
)

SET(gcutil_SRCS
	ByteArray.cxx
	Config.cxx
	DataStream.cxx
	DataStreamUtils.cxx
	GSException.cxx
	Logger.cxx
	GSThread.cxx
	ControlledThread.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libgcutil SHARED ${gcutil_SRCS})
	target_link_libraries(libgcutil ${BRLCAD_BU_LIBRARY} ${TCL_LIBRARIES} ${QT_LIBRARIES} uuid)
	set_target_properties(libgcutil PROPERTIES PREFIX "")
	install(TARGETS libgcutil DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgcutil-static STATIC ${gcutil_SRCS})
	target_link_libraries(libgcutil-static ${BRLCAD_BU_LIBRARY} ${TCL_LIBRARIES} ${QT_LIBRARIES} uuid)
	IF(NOT WIN32)
		set_target_properties(libgcutil-static PROPERTIES PREFIX "")
		set_target_properties(libgcutil-static PROPERTIES OUTPUT_NAME "libgcutil")
	ELSE(NOT WIN32)
		set_target_properties(libgcutil-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libgcutil-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

SET(util_inst_HDRS
	Config.h
	DataStreamUtils.h
	GSException.h
	Logger.h
	GSThread.h
	ControlledThread.h
)
INSTALL(FILES ${util_inst_HDRS} DESTINATION include/gcutil)
