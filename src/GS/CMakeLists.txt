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
#	@file geomcore/src/GS/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

PROJECT(GEOMETRY_SERVICE)

include_directories(
	${QT_INCLUDE_DIR}
	${BRLCAD_INC_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	)

SET(geomserv_SRCS
	GeometryService.cxx
	DataManager.cxx
	FileDataSource.cxx
	DbObjectManifest.cxx
	DbObject.cxx
	GeometryProcessor.cxx
	Session.cxx
	SessionManager.cxx
	Account.cxx
	AccountManager.cxx
	GSClient.cxx
	GSCmdLineClient.cxx
	cmds/ClientCmdRegistry.cxx
	cmds/AbstractClientCmd.cxx
	cmds/HelpCmd.cxx
	cmds/LoginCmd.cxx
	cmds/LogoutCmd.cxx
	cmds/ShutdownCmd.cxx
	cmds/ExitCmd.cxx
	cmds/PingCmd.cxx
	)
IF(BUILD_SHARED_LIBS)
	add_library(libgeomserv SHARED ${geomserv_SRCS})
	target_link_libraries(libgeomserv ${QT_LIBRARIES} libNet libgcutil)
	set_target_properties(libgeomserv PROPERTIES PREFIX "")
	install(TARGETS libgeomserv DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgeomserv-static STATIC ${geomserv_SRCS})
	target_link_libraries(libgeomserv-static ${QT_LIBRARIES} libNet libgcutil)
	IF(NOT WIN32)
		set_target_properties(libgeomserv-static PROPERTIES PREFIX "")
		set_target_properties(libgeomserv-static PROPERTIES OUTPUT_NAME "${libname}")
	ELSE(NOT WIN32)
		set_target_properties(libgeomserv-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libgeomserv-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

SET(geomserv_inst_HDRS
	Session.h
	SessionManager.h
	Account.h
	AccountManager.h
	GeometryService.h
	DataManager.h
	IDataSource.h
	FileDataSource.h
	DbObject.h
	GSClient.h
	GSCmdLineClient.h
	ClientCmdRegistry.h
	AbstractClientCmd.h
	HelpCmd.h
	LoginCmd.h
	LogoutCmd.h
	ShutdownCmd.h
	ExitCmd.h
	)
INSTALL(FILES ${geomserv_inst_HDRS} DESTINATION include)

add_executable(geoserv geoserv.cxx)
target_link_libraries(geoserv libgcutil libNet libgeomserv ${QT_LIBRARIES})
install(TARGETS geoserv DESTINATION bin)
file(COPY geoserv.config DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


add_executable(geoclient geoclient.cxx)
target_link_libraries(geoclient libgcutil libNet libgeomserv ${QT_LIBRARIES})
install(TARGETS geoclient DESTINATION bin)
file(COPY geoclient.config DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


