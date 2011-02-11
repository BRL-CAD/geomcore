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
#	@file geomcore/cmake/FindBRLCAD.cmake
#
# 	Try to find brlcad libraries.
# 	Once done, this will define:
#
#  	BRLCAD_FOUND - system has BRL-CAD
#  	BRLCAD_VERSION - the BRL-CAD version string
#  	BRLCAD_INCLUDE_DIRS - the BRL-CAD include directories
#  	BRLCAD_LIBRARIES - link these to use the BRL-CAD Libraries
#
#     BRLCAD_BU_LIBRARY - BRL-CAD Utility library
#     BRLCAD_BN_LIBRARY - BRL-CAD Numerical library
#     BRLCAD_RT_LIBRARY - BRL-CAD Raytracing library
#     BRLCAD_DM_LIBRARY - BRL-CAD Display Manager library
#     BRLCAD_FB_LIBRARY - BRL-CAD Frame Buffer library
#     BRLCAD_FFT_LIBRARY - BRL-CAD FFT library
#     BRLCAD_ANALYZE_LIBRARY - BRL-CAD Analysis library
#     BRLCAD_GCV_LIBRARY - BRL-CAD Geometry Conversion library
#     BRLCAD_ICV_LIBRARY - BRL-CAD Image Conversion library
#     BRLCAD_WDB_LIBRARY - BRL-CAD Write Database library
#     BRLCAD_GED_LIBRARY - BRL-CAD Geometry Editing library
#     BRLCAD_MULTISPECTRAL_LIBRARY - BRL-CAD multispectral library
#     BRLCAD_OPTICAL_LIBRARY - BRL-CAD optical library
#     BRLCAD_GED_LIBRARY - BRL-CAD Geometry Editing library
#     BRLCAD_PKG_LIBRARY - BRL-CAD libpkg
#     BRLCAD_CURSOR_LIBRARY - libcursor
#     BRLCAD_ORLE_LIBRARY - liborle
#     BRLCAD_RENDER_LIBRARY - librender
#     BRLCAD_TIE_LIBRARY - libtie
#     BRLCAD_SYSV_LIBRARY - libsysv
#     BRLCAD_TERMIO_LIBRARY - libtermio
#     BRLCAD_EXPPP_LIBRARY - SCL libexppp library
#     BRLCAD_EXPRESS_LIBRARY - SCL libexpress library
#     BRLCAD_STEPCORE_LIBRARY - SCL core library
#     BRLCAD_STEPDAI_LIBRARY - SCL dai library
#     BRLCAD_STEPEDITOR_LIBRARY - SCL editor library
#     BRLCAD_STEPUTILS_LIBRARY - SCL utils library
#     BRLCAD_OPENNURBS_LIBRARY - openNURBS library
#     BRLCAD_UTAHRLE_LIBRARY - libutahrle
#
#  In addition to the above variables, which are essentially unique
#  to BRL-CAD, this routine will look for local copies of libraries
#  installed with BRL-CAD and return  their results as would a 
#  standard find_package for that library (if one exists).  The 
#  libraries searched for:
#
#  libregex
#  libpng
#  zlib
#  libtermlib
#  tcl/tk
#  incrTcl
#  Togl
# 
#	$Revision:  $
#	$Author:  $
#
#########################################################################

SET(BRLCAD_ROOT "$ENV{BRLCAD_ROOT}")
IF(BRLCAD_BASE_DIR AND BRLCAD_ROOT)
	MESSAGE("Warning - BRLCAD_ROOT was found but is overridden by BRLCAD_BASE_DIR")
ELSE(BRLCAD_BASE_DIR AND BRLCAD_ROOT)
	IF(BRLCAD_ROOT)
		SET(BRLCAD_BASE_DIR ${BRLCAD_ROOT})
	ENDIF(BRLCAD_ROOT)
ENDIF(BRLCAD_BASE_DIR AND BRLCAD_ROOT)

#First, find the install directories.
IF(BRLCAD_BASE_DIR)
    #if BRLCAD_BASE_DIR is set, then this makes it easy!
    SET(BRLCAD_BIN_DIR "${BRLCAD_BASE_DIR}/bin")
    SET(BRLCAD_INC_DIRS "${BRLCAD_BASE_DIR}/include" "${BRLCAD_BASE_DIR}/include/brlcad" "${BRLCAD_BASE_DIR}/include/openNURBS")
    SET(BRLCAD_LIB_DIR "${BRLCAD_BASE_DIR}/lib")
ELSE(BRLCAD_BASE_DIR)
	 #try looking for BRL-CAD's brlcad-config - it should give us
	 #a location
    FIND_PATH(BRLCAD_BIN_DIR brlcad-config)
	 IF(BRLCAD_BIN_PATH)
		 EXEC_PROGRAM(${BRLCAD_BIN_DIR} ARGS --prefix OUTPUT_VARIABLE BRLCAD_BASE_DIR)
	 ENDIF(BRLCAD_BIN_PATH)
	 IF(NOT BRLCAD_BASE_DIR)
		 SET(BRLCAD_HEADERS_DIR_CANDIDATES 
			 /usr/brlcad/include/brlcad 
			 /usr/local/brlcad/include/brlcad
			 )
		 #Look for headers if brlcad-config fails
		 FIND_PATH(BRLCAD_HEADERS_DIR NAMES bu.h bn.h rt.h PATHS ${BRLCAD_HEADERS_DIR_CANDIDATES})
		 IF(BRLCAD_HEADERS_DIR)
			 GET_FILENAME_COMPONENT(BRLCAD_BASE_DIR ${BRLCAD_HEADERS_DIR} PATH)
		 ENDIF(BRLCAD_HEADERS_DIR)
	 ENDIF(NOT BRLCAD_BASE_DIR)

	 IF(NOT BRLCAD_BASE_DIR)
		 MESSAGE(FATAL_ERROR "\nCould not find BRL-CAD root directory - please set BRLCAD_BASE_DIR in CMake")
	 ENDIF(NOT BRLCAD_BASE_DIR)

    #Find include directories (aka more than one)
    SET(HEADERS_TO_SEARCH_FOR brlcad/bu.h bu.h opennurbs.h )
   
	 SET(INCLUDE_PATH_LIST "$ENV{PATH}")
	 STRING(REGEX REPLACE "/bin:" "/include:" INCLUDE_PATH_LIST "${INCLUDE_PATH_LIST}")
	 STRING(REGEX REPLACE "/bin$" "/include" INCLUDE_PATH_LIST "${INCLUDE_PATH_LIST}")	
	 STRING(REGEX REPLACE ":" ";" INCLUDE_PATH_LIST "${INCLUDE_PATH_LIST}")	 

    FOREACH (tHead ${HEADERS_TO_SEARCH_FOR})
            
		 FIND_PATH(_HEADER_DIR_${tHead} ${tHead} ${INCLUDE_PATH_LIST})
          
        IF(_HEADER_DIR_${tHead})
			  SET(BRLCAD_INC_DIRS ${BRLCAD_INC_DIRS} ${_HEADER_DIR_${tHead}} ${_HEADER_DIR_${tHead}}/brlcad ${_HEADER_DIR_${tHead}}/openNURBS)
            SET(BRLCAD_HEADERS_FOUND ${BRLCAD_HEADERS_FOUND} ${tHead})        
        ELSE(_HEADER_DIR_${tHead})
            SET(BRLCAD_HEADERS_NOTFOUND ${BRLCAD_HEADERS_NOTFOUND} ${tHead})        
        ENDIF(_HEADER_DIR_${tHead})

    ENDFOREACH (tHead ${HEADERS_TO_SEARCH_FOR})
    
    IF(NOT BRLCAD_INC_DIRS)
		 MESSAGE(STATUS "\t\tCould not find BRLCAD include directories anywhere in paths: ${INCLUDE_PATH_LIST}")
    	RETURN()
    ENDIF(NOT BRLCAD_INC_DIRS)
 
	 SET(LIB_PATH_LIST "$ENV{PATH}")
	 STRING(REGEX REPLACE "/bin:" "/lib:" LIB_PATH_LIST "${LIB_PATH_LIST}")	
	 STRING(REGEX REPLACE "/bin$" "/lib" LIB_PATH_LIST "${LIB_PATH_LIST}")	
	 STRING(REGEX REPLACE ":" ";" LIB_PATH_LIST "${LIB_PATH_LIST}")	 
        
	 FIND_PATH(BRLCAD_LIB_DIR "libbu${CMAKE_SHARED_LIBRARY_SUFFIX}" ${LIB_PATH_LIST})
    
    IF(NOT BRLCAD_LIB_DIR)
		 MESSAGE(STATUS "\t\tCould not find brlcad library directory in: ${LIB_PATH_LIST}")
    	RETURN()
    ENDIF(NOT BRLCAD_LIB_DIR)

ENDIF(BRLCAD_BASE_DIR)



#Attempt to get brlcad version.
FIND_PROGRAM(BRLCAD_CONFIGEXE brlcad-config)
IF(BRLCAD_CONFIGEXE)
	EXECUTE_PROCESS(COMMAND ${BRLCAD_CONFIGEXE} --version OUTPUT_VARIABLE BRLCAD_VERSION)
	STRING(STRIP ${BRLCAD_VERSION} BRLCAD_VERSION)
	
	IF(BRLCAD_VERSION)
    	STRING(REGEX REPLACE "([0-9]+)\\.[0-9]+\\.[0-9]+" "\\1" BRLCAD_MAJOR_VERSION "${BRLCAD_VERSION}")
    	STRING(REGEX REPLACE "[0-9]+\\.([0-9]+)\\.[0-9]+" "\\1" BRLCAD_MINOR_VERSION "${BRLCAD_VERSION}")
    	STRING(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" BRLCAD_PATCH_VERSION "${BRLCAD_VERSION}")
        SET(BRLCAD_VERSION_FOUND TRUE)
    ELSEIF(BRLCAD_VERSION)
        MESSAGE(STATUS "\t\t'brlcad-config --version' was found and executed, but produced no output.")
        SET(BRLCAD_VERSION_FOUND FALSE)
    ENDIF(BRLCAD_VERSION)

ELSE(BRLCAD_CONFIGEXE)
    MESSAGE(STATUS "\t\tCould not locate 'brlcad-config'.")
    SET(BRLCAD_VERSION_FOUND FALSE)
ENDIF(BRLCAD_CONFIGEXE)

#TODO need to make the BRLCAD version checking a requirement for coreInterface, but nothing else.
#TODO figure out why brlcad-config isn't present on Windows.
##########################################################################
#Search for Libs

SET(LIBS_TO_SEARCH_FOR 
	analyze
	bn
	brlcad
	bu
	cursor
	dm
	exppp
	express
	fb
	fft
	gcv
	ged
	icv
	multispectral
	openNURBS
	optical
	orle
	pkg
	png14
	png
	regex
	render
	rt
	stepcore
	stepdai
	stepeditor
	steputils
	sysv
	termio
	tie
	utahrle
	wdb
)

MESSAGE(STATUS "BRLCAD_LIB_DIR: ${BRLCAD_LIB_DIR}")
MESSAGE(STATUS "LIB_EXT: ${LIB_EXT}")

FOREACH (tlib ${LIBS_TO_SEARCH_FOR})
	 FIND_LIBRARY(_BRLCAD_LIBRARY_${tlib} ${tlib} ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
   
    IF(_BRLCAD_LIBRARY_${tlib})
        SET(BRLCAD_LIBRARIES ${BRLCAD_LIBRARIES} ${_BRLCAD_LIBRARY_${tlib}})
        SET(BRLCAD_LIBRARIES_FOUND ${BRLCAD_LIBRARIES_FOUND} ${tlib})        
    ELSE(_BRLCAD_LIBRARY_${tlib})
        SET(BRLCAD_LIBRARIES_NOTFOUND ${BRLCAD_LIBRARIES_NOTFOUND} ${tlib})        
    ENDIF(_BRLCAD_LIBRARY_${tlib})

ENDFOREACH (tlib ${LIBS_TO_SEARCH_FOR})

##########################################################################
#Print status
MESSAGE(STATUS "")
MESSAGE(STATUS "\t\t Discovered BRLCAD Version ${BRLCAD_VERSION}")
MESSAGE(STATUS "\t\t BRLCAD_BIN_DIR:     ${BRLCAD_BIN_DIR}")
MESSAGE(STATUS "\t\t BRLCAD_INC_DIRS:    ${BRLCAD_INC_DIRS}")
MESSAGE(STATUS "\t\t BRLCAD_LIB_DIR:     ${BRLCAD_LIB_DIR}")
MESSAGE(STATUS "\t\t BRLCAD_LIBRARIES_FOUND:    ${BRLCAD_LIBRARIES_FOUND}")
MESSAGE(STATUS "\t\t BRLCAD_LIBRARIES_NOTFOUND: ${BRLCAD_LIBRARIES_NOTFOUND}")
MESSAGE(STATUS "\t\t BRLCAD_CONFIGEXE: ${BRLCAD_CONFIGEXE}")
IF(BRLCAD_CONFIGEXE)
    MESSAGE(STATUS "\t\t\t BRLCAD_MAJOR_VERSION: ${BRLCAD_MAJOR_VERSION}")
    MESSAGE(STATUS "\t\t\t BRLCAD_MINOR_VERSION: ${BRLCAD_MINOR_VERSION}")
    MESSAGE(STATUS "\t\t\t BRLCAD_PATCH_VERSION: ${BRLCAD_PATCH_VERSION}")
ENDIF(BRLCAD_CONFIGEXE)
MESSAGE(STATUS "")

#Set found flag
SET(BRLCAD_FOUND TRUE)
