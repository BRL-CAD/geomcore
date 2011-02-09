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
#	@file geomcore/tests/libNet/CMakeLists.txt
#
#	$Revision:  $
#	$Author:  $
#
##########################################################################

include_directories(
    ${QT_INCLUDE_DIR}
    ${TCL_INCLUDE_DIRS}
    ${BRLCAD_INC_DIRS}
)

add_executable(gNetMsgSerialtest netMsgSerialTest.cxx)
target_link_libraries(gNetMsgSerialtest libNet libPkgCpp libgcutil ${QT_LIBRARIES} ${_BRLCAD_LIBRARY_pkg} ${_BRLCAD_LIBRARY_bu})

add_executable(glibNetTest libNetTest.cxx)
target_link_libraries(glibNetTest libNet libPkgCpp libgcutil ${QT_LIBRARIES} ${_BRLCAD_LIBRARY_pkg} ${_BRLCAD_LIBRARY_bu})
