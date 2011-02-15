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
#	@file geomcore/src/libPkgCpp/CMakeLists.txt
#
#    C++ layer for LibPkg Library
#
##########################################################################

include_directories(
    ${TCL_INCLUDE_DIRS}
	 ${BRLCAD_INCLUDE_DIRS}
)

SET(libPkgCpp_SRCS
	PkgClient.cxx
	PkgTcpClient.cxx
	PkgUdpClient.cxx
	PkgServer.cxx
	PkgTcpServer.cxx
	PkgUdpServer.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libPkgCpp SHARED ${libPkgCpp_SRCS})
	target_link_libraries(libPkgCpp ${TCL_LIBRARY} ${BRLCAD_PKG_LIBRARY} ${BRLCAD_BU_LIBRARY})
	set_target_properties(libPkgCpp PROPERTIES PREFIX "")
	install(TARGETS libPkgCpp DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libPkgCpp-static STATIC ${libPkgCpp_SRCS})
	target_link_libraries(libPkgCpp-static ${TCL_LIBRARY} ${BRLCAD_PKG_LIBRARY} ${BRLCAD_BU_LIBRARY})
	IF(NOT WIN32)
		set_target_properties(libPkgCpp-static PROPERTIES PREFIX "")
		set_target_properties(libPkgCpp-static PROPERTIES OUTPUT_NAME "libPkgCpp")
	ELSE(NOT WIN32)
		set_target_properties(libPkgCpp-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libPkgCpp-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

SET(libPkgCPP_inst_HDRS
	pkgcppcommon.h
	PkgClient.h
	PkgTcpClient.h
	PkgUdpClient.h
	PkgServer.h
	PkgTcpServer.h
	PkgUdpServer.h
)
INSTALL(FILES ${libPkgCPP_inst_HDRS} DESTINATION include)
