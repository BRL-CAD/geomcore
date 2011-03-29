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
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
	${LIBGE_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}
)

SET(geomserv_SRCS
	GeometryService.cxx
	DataManager.cxx
	FileDataSource.cxx
	SvnDataSource.cxx
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
	target_link_libraries(libgeomserv libNet libgcutil ge bu rt )
	set_target_properties(libgeomserv PROPERTIES PREFIX "")
	install(TARGETS libgeomserv DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgeomserv-static STATIC ${geomserv_SRCS})
	target_link_libraries(libgeomserv-static libNet libgcutil ge bu rt )
	IF(NOT WIN32)
		set_target_properties(libgeomserv-static PROPERTIES PREFIX "")
		set_target_properties(libgeomserv-static PROPERTIES OUTPUT_NAME "libgeomserv")
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
	IDataSource.h
	DataManager.h
	FileDataSource.h
	SvnDataSource.h
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

add_executable(geomserv geomserv.cxx)
target_link_libraries(geomserv libgcutil libNet libgeomserv)
install(TARGETS geomserv DESTINATION bin)
file(COPY geomserv.config DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


add_executable(geomclient geomclient.cxx)
target_link_libraries(geomclient libgcutil libNet libgeomserv)
install(TARGETS geomclient DESTINATION bin)
file(COPY geomclient.config DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})


