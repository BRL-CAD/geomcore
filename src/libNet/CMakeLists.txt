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
#	@file geomcore/src/libNet/CMakeLists.txt
#
##########################################################################

include_directories(
    ${QT_INCLUDE_DIR}
    ${TCL_INCLUDE_DIRS}
	 ${BRLCAD_INCLUDE_DIRS}
)

SET(libNet_SRCS
	Portal.cxx
	PortalManager.cxx
	NetMsgFactory.cxx
	NetMsgRouter.cxx
	netMsg/NetMsg.cxx
	netMsg/TypeOnlyMsg.cxx
	netMsg/GenericOneStringMsg.cxx
	netMsg/GenericOneByteMsg.cxx
	netMsg/GenericTwoBytesMsg.cxx
	netMsg/GenericFourBytesMsg.cxx
	netMsg/GenericEightBytesMsg.cxx
	netMsg/GenericMultiByteMsg.cxx
	netMsg/FailureMsg.cxx
	netMsg/SuccessMsg.cxx
	netMsg/RemoteNodenameSetMsg.cxx
 	netMsg/NewSessionReqMsg.cxx
 	netMsg/SessionInfoMsg.cxx
	netMsg/NewNodeOnNetMsg.cxx
	netMsg/GeometryReqMsg.cxx
	netMsg/GeometryManifestMsg.cxx
	netMsg/GeometryChunkMsg.cxx
	netMsg/PingMsg.cxx
	netMsg/PongMsg.cxx
	RouteMsgJob.cxx
)

IF(BUILD_SHARED_LIBS)
	add_library(libNet SHARED ${libNet_SRCS})
	target_link_libraries(libNet libgeomengine libJob libgcutil libEvent libPkgCpp ${QT_LIBRARIES})
	set_target_properties(libNet PROPERTIES PREFIX "")
	install(TARGETS libNet DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libNet-static STATIC ${libNet_SRCS})
	target_link_libraries(libNet-static libgeomengine libJob libgcutil libEvent libPkgCpp ${QT_LIBRARIES})
	IF(NOT WIN32)
		set_target_properties(libNet-static PROPERTIES PREFIX "")
	ELSE(NOT WIN32)
		set_target_properties(libNet-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libNet-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)

#Set INST Headers
SET(libNet_inst_HDRS
	Portal.h
	PortalManager.h
	NetMsgTypes.h
	INetMsgHandler.h
	NetMsgFactory.h
	NetMsgRouter.h
	NetMsg.h
	TypeOnlyMsg.h
	RemoteNodenameSetMsg.h
	GenericOneStringMsg.h
	GenericOneByteMsg.h
	GenericTwoBytesMsg.h
	GenericFourBytesMsg.h
	GenericEightBytesMsg.h
	GenericMultiByteMsg.h
	FailureMsg.h
	SuccessMsg.h
	NewSessionReqMsg.h
	SessionInfoMsg.h
	NewNodeOnNetMsg.h
	GeometryReqMsg.h
	GeometryManifestMsg.h
	GeometryChunkMsg.h
	PingMsg.h
	PongMsg.h
	RouteMsgJob.h
)
INSTALL(FILES ${libNet_inst_HDRS} DESTINATION include)
