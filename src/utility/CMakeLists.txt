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
    ${TCL_INCLUDE_DIRS}
    ${BRLCAD_INCLUDE_DIRS}
)

SET(gcutil_SRCS
	ByteArray.cxx
	Config.cxx
	ControlledThread.cxx
	DataStream.cxx
	GSException.cxx
	GSThread.cxx
	GSUuid.cxx
	Logger.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libgcutil SHARED ${gcutil_SRCS})
	target_link_libraries(libgcutil ${BRLCAD_BU_LIBRARY} ${TCL_LIBRARIES} uuid)
	set_target_properties(libgcutil PROPERTIES PREFIX "")
	install(TARGETS libgcutil DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgcutil-static STATIC ${gcutil_SRCS})
	target_link_libraries(libgcutil-static ${BRLCAD_BU_LIBRARY} ${TCL_LIBRARIES} uuid)
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
	ControlledThread.h
	DataStreamUtils.h
	GSException.h
	GSThread.h
	GSUuid.h
	Logger.h
)
INSTALL(FILES ${util_inst_HDRS} DESTINATION include/gcutil)
