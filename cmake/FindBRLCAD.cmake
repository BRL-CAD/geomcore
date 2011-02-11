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
#     BRLCAD_TERMIO_LIBRARY - libtermio (non WIN32 systems)
#
#  Technically these are external but we need the versions
#  tweaked for BRL-CAD.  If a developer wishes to use the
#  BRL-CAD altered versions of these libraries to satisfy a
#  "generic" request for the library, they'll need to assign
#  the results of these variables to the non-BRL-CAD specific 
#  variables they are using
#
#     BRLCAD_EXPPP_LIBRARY - SCL libexppp library
#     BRLCAD_EXPRESS_LIBRARY - SCL libexpress library
#     BRLCAD_STEPCORE_LIBRARY - SCL core library
#     BRLCAD_STEPDAI_LIBRARY - SCL dai library
#     BRLCAD_STEPEDITOR_LIBRARY - SCL editor library
#     BRLCAD_STEPUTILS_LIBRARY - SCL utils library
#     BRLCAD_OPENNURBS_LIBRARY - openNURBS library
#
#  In addition to the above variables, which are essentially unique
#  to BRL-CAD, this routine will look for local copies of libraries
#  installed with BRL-CAD and return their results as would a 
#  standard find_package for that library (if one exists).  The 
#  distinction between these libraries and those above is that these
#  libraries do not have known modifications required by BRL-CAD that
#  preclude system libraries from substituting for them, although
#  if BRL-CAD was compiled against local versions of these it is
#  not guaranteed (or in some cases expected) that a found system
#  version will successfully replace the local copy.
#
#  libregex
#  libpng
#  zlib
#  libtermlib
#  tcl/tk
#  incrTcl
#  Togl
#  utahrle
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
IF(NOT BRLCAD_BASE_DIR)
	#try looking for BRL-CAD's brlcad-config - it should give us
	#a location for bin, and the parent will be the root dir
	IF(NOT BRLCAD_BASE_DIR)
		FIND_PROGRAM(BRLCAD_CONFIGEXE brlcad-config)
		IF(BRLCAD_CONFIGEXE)
			EXEC_PROGRAM(brlcad-config ARGS --prefix OUTPUT_VARIABLE BRLCAD_BASE_DIR)
		ENDIF(BRLCAD_CONFIGEXE)
	ENDIF(NOT BRLCAD_BASE_DIR)

	#if that didn't work, see if we can find brlcad-config and use the parent path
	IF(NOT BRLCAD_BASE_DIR)
		FIND_PATH(BRLCAD_BIN_DIR brlcad-config)
		IF(BRLCAD_BIN_DIR)
			GET_FILENAME_COMPONENT(BRLCAD_BASE_DIR ${BRLCAD_BIN_DIR} PATH)
		ENDIF(BRLCAD_BIN_DIR)
	ENDIF(NOT BRLCAD_BASE_DIR)

	#Look for headers if we come up empty with brlcad-config
	IF(NOT BRLCAD_BASE_DIR)
		SET(BRLCAD_HEADERS_DIR_CANDIDATES 
			/usr/brlcad/include/brlcad 
			/usr/local/brlcad/include/brlcad
			)
		FIND_PATH(BRLCAD_HEADERS_DIR NAMES bu.h bn.h rt.h PATHS ${BRLCAD_HEADERS_DIR_CANDIDATES})
		IF(BRLCAD_HEADERS_DIR)
			GET_FILENAME_COMPONENT(BRLCAD_BASE_DIR ${BRLCAD_HEADERS_DIR} PATH)
		ENDIF(BRLCAD_HEADERS_DIR)
	ENDIF(NOT BRLCAD_BASE_DIR)

	IF(NOT BRLCAD_BASE_DIR)
		MESSAGE(FATAL_ERROR "\nCould not find BRL-CAD root directory - please set BRLCAD_BASE_DIR in CMake")
	ENDIF(NOT BRLCAD_BASE_DIR)
ENDIF(NOT BRLCAD_BASE_DIR)

#Find include directories
IF(NOT BRLCAD_HEADERS_DIR)
	FIND_PATH(BRLCAD_HEADERS_DIR NAMES bu.h HINTS ${BRLCAD_BASE_DIR} PATH_SUFFIXES include/brlcad)
ENDIF(NOT BRLCAD_HEADERS_DIR)
FIND_PATH(BRLCAD_OPENNURBS_HEADERS_DIR NAMES opennurbs.h HINTS ${BRLCAD_BASE_DIR} PATH_SUFFIXES include/openNURBS include/opennurbs)
SET(BRLCAD_INC_DIRS ${BRLCAD_HEADERS_DIR} ${BRLCAD_OPENNURBS_HEADERS_DIR})

#Find library directory
FIND_PATH(BRLCAD_LIB_DIR "libbu${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS ${BRLCAD_BASE_DIR} PATH_SUFFIXES lib libs)

#Find binary directory
IF(NOT BRLCAD_BIN_DIR)
	FIND_PATH(BRLCAD_BIN_DIR brlcad-config PATHS ${BRLCAD_BASE_DIR} PATH_SUFFIXES bin)
ENDIF(NOT BRLCAD_BIN_DIR)

#Attempt to get brlcad version.
IF(NOT BRLCAD_CONFIGEXE)
	FIND_PROGRAM(BRLCAD_CONFIGEXE brlcad-config)
ENDIF(NOT BRLCAD_CONFIGEXE)
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
	MESSAGE(STATUS "Could not locate 'brlcad-config' - no BRL-CAD version available")
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
