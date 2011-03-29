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
#	@file geomcore/cmake/FindLIBGE.cmake
#
# 	Try to find LIBGE libraries.
# 	Once done, this will define:
#
#  	LIBGE_FOUND - system has LibGE
#  	LIBGE_INCLUDE_DIRS - the BRL-CAD include directories
#  	LIBGE_LIBRARIES - link these to use the BRL-CAD Libraries
#
#     LIBGE_GE_LIBRARY - The Geometry Engine library
# 
#########################################################################

SET(LIBGE_HEADERS_DIR_CANDIDATES 
	/usr/brlcad/include 
	/usr/local/brlcad/include
	/usr/brlcad/include/brlcad 
	/usr/local/brlcad/include/brlcad
	${LIBGE_BASE_DIR}/include
)
FIND_PATH(LIBGE_HEADERS_DIR NAMES GeometryEngine.h PATHS ${LIBGE_HEADERS_DIR_CANDIDATES})

SET(LIBGE_LIB_DIR_CANDIDATES 
	/usr/brlcad/lib 
	/usr/local/brlcad/lib
	${LIBGE_BASE_DIR}/lib
)
#FIND_LIBRARY(LIBGE_LIBRARY NAMES ge libge PATHS ${LIBGE_LIB_DIR_CANDIDATES} NO_SYSTEM_PATH)


SET(RT3_LIBS_SEARCH_LIST
	ge
)

FOREACH(brl_lib ${RT3_LIBS_SEARCH_LIST})
	STRING(TOUPPER ${brl_lib} LIBCORE)
	FIND_LIBRARY(LIBGE_${LIBCORE}_LIBRARY NAMES ${brl_lib} lib${brl_lib} PATHS ${LIBGE_LIB_DIR_CANDIDATES} NO_SYSTEM_PATH)
   IF(LIBGE_${LIBCORE}_LIBRARY)
		SET(LIBGE_LIBRARIES ${LIBGE_LIBRARIES} ${LIBGE_${LIBCORE}_LIBRARY})
		SET(LIBGE_FOUND TRUE)
	ELSE(LIBGE_${LIBCORE}_LIBRARY)
		SET(LIBGE_LIBRARIES_NOTFOUND ${LIBGE_LIBRARIES_NOTFOUND} ${brl_lib})
	ENDIF(LIBGE_${LIBCORE}_LIBRARY)
ENDFOREACH(brl_lib ${BRL-CAD_LIBS_SEARCH_LIST})

if(LIBGE_FOUND)
	MESSAGE(STATUS "Found libGE at: ${LIBGE_GE_LIBRARY}")
endif(LIBGE_FOUND)
