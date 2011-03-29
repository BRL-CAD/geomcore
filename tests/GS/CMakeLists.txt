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
#	@file geomcore/tests/GS/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

include_directories( 
    ${TCL_INCLUDE_DIRS}
    ${BRLCAD_INCLUDE_DIRS}
    ${LIBGE_INCLUDE_DIR}
)

add_executable(gsTest GeometryServiceTest.cxx)
target_link_libraries(gsTest libgeomserv libNet ${BRLCAD_PKG_LIBRARY} ${BRLCAD_BU_LIBRARY} ${LIBGE_GE_LIBRARY})

add_executable(fileDataSourceTest FileDataSourceTest.cxx)
target_link_libraries(fileDataSourceTest libgeomserv ${BRLCAD_LIBRARIES})

add_executable(SimpleCoreInterfaceTest SimpleCoreInterfaceTest.cxx)
target_link_libraries(SimpleCoreInterfaceTest libgeomserv ${BRLCAD_LIBRARIES})

