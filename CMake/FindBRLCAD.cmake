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
#     BRLCAD_ANALYZE_LIBRARY - BRL-CAD Analysis library
#     BRLCAD_BN_LIBRARY - BRL-CAD Numerical library
#     BRLCAD_BU_LIBRARY - BRL-CAD Utility library
#     BRLCAD_CURSOR_LIBRARY - libcursor
#     BRLCAD_DM_LIBRARY - BRL-CAD Display Manager library
#     BRLCAD_FB_LIBRARY - BRL-CAD Frame Buffer library
#     BRLCAD_FFT_LIBRARY - BRL-CAD FFT library
#     BRLCAD_GCV_LIBRARY - BRL-CAD Geometry Conversion library
#     BRLCAD_GED_LIBRARY - BRL-CAD Geometry Editing library
#     BRLCAD_ICV_LIBRARY - BRL-CAD Image Conversion library
#     BRLCAD_MULTISPECTRAL_LIBRARY - BRL-CAD multispectral library
#     BRLCAD_OPTICAL_LIBRARY - BRL-CAD optical library
#     BRLCAD_ORLE_LIBRARY - liborle
#     BRLCAD_PKG_LIBRARY - BRL-CAD libpkg
#     BRLCAD_RENDER_LIBRARY - librender
#     BRLCAD_RT_LIBRARY - BRL-CAD Raytracing library
#     BRLCAD_SYSV_LIBRARY - libsysv
#     BRLCAD_TCLCAD_LIBRARY - libtclcad
#     BRLCAD_TERMIO_LIBRARY - libtermio (non WIN32 systems)
#     BRLCAD_WDB_LIBRARY - BRL-CAD Write Database library
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
#  Togl
#  utahrle
#
#########################################################################

set(BRLCAD_ROOT "$ENV{BRLCAD_ROOT}")
if(BRLCAD_BASE_DIR AND BRLCAD_ROOT)
  message("Warning - BRLCAD_ROOT was found but is overridden by BRLCAD_BASE_DIR")
else(BRLCAD_BASE_DIR AND BRLCAD_ROOT)
  if(BRLCAD_ROOT)
    set(BRLCAD_BASE_DIR ${BRLCAD_ROOT})
  endif(BRLCAD_ROOT)
endif(BRLCAD_BASE_DIR AND BRLCAD_ROOT)

#First, find the install directories.
if(NOT BRLCAD_BASE_DIR)
  #try looking for BRL-CAD's brlcad-config - it should give us
  #a location for bin, and the parent will be the root dir
  if(NOT BRLCAD_BASE_DIR)
    find_program(BRLCAD_CONFIGEXE brlcad-config)
    if(BRLCAD_CONFIGEXE)
      exec_program(brlcad-config ARGS --prefix OUTPUT_VARIABLE BRLCAD_BASE_DIR)
    endif(BRLCAD_CONFIGEXE)
  endif(NOT BRLCAD_BASE_DIR)

  #if that didn't work, see if we can find brlcad-config and use the parent path
  if(NOT BRLCAD_BASE_DIR)
    find_path(BRLCAD_BIN_DIR brlcad-config)
    if(BRLCAD_BIN_DIR)
      get_filename_component(BRLCAD_BASE_DIR ${BRLCAD_BIN_DIR} PATH)
    endif(BRLCAD_BIN_DIR)
  endif(NOT BRLCAD_BASE_DIR)

  #Look for headers if we come up empty with brlcad-config
  if(NOT BRLCAD_BASE_DIR)
    set(BRLCAD_HEADERS_DIR_CANDIDATES
      /usr/brlcad/include/brlcad
      /usr/local/brlcad/include/brlcad
      )
    find_path(BRLCAD_HEADERS_DIR NAMES bu.h bn.h rt.h PATHS ${BRLCAD_HEADERS_DIR_CANDIDATES})
    if(BRLCAD_HEADERS_DIR)
      get_filename_component(BRLCAD_BASE_DIR ${BRLCAD_HEADERS_DIR} PATH)
    endif(BRLCAD_HEADERS_DIR)
  endif(NOT BRLCAD_BASE_DIR)

  if(NOT BRLCAD_BASE_DIR)
    message(FATAL_ERROR "\nCould not find BRL-CAD root directory - please set BRLCAD_BASE_DIR in CMake")
  endif(NOT BRLCAD_BASE_DIR)
endif(NOT BRLCAD_BASE_DIR)

#Find include directories
if(NOT BRLCAD_HEADERS_DIR)
  find_path(BRLCAD_HEADERS_DIR NAMES bu.h HINTS ${BRLCAD_BASE_DIR} PATH_SUFFIXES include/brlcad)
  get_filename_component(BRLCAD_HEADERS_PARENT_DIR ${BRLCAD_HEADERS_DIR} PATH)
endif(NOT BRLCAD_HEADERS_DIR)
find_path(BRLCAD_OPENNURBS_HEADERS_DIR NAMES opennurbs.h HINTS ${BRLCAD_BASE_DIR} PATH_SUFFIXES include/openNURBS include/opennurbs)
set(BRLCAD_INCLUDE_DIRS ${BRLCAD_HEADERS_PARENT_DIR} ${BRLCAD_HEADERS_DIR} ${BRLCAD_OPENNURBS_HEADERS_DIR})
set(BRLCAD_INCLUDE_DIRS ${BRLCAD_INCLUDE_DIRS} CACHE STRING "BRL-CAD include directories")

#Find library directory
find_path(BRLCAD_LIB_DIR "libbu${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS ${BRLCAD_BASE_DIR} PATH_SUFFIXES lib libs)

#Find binary directory
if(NOT BRLCAD_BIN_DIR)
  find_path(BRLCAD_BIN_DIR brlcad-config PATHS ${BRLCAD_BASE_DIR} PATH_SUFFIXES bin)
endif(NOT BRLCAD_BIN_DIR)

#Attempt to get brlcad version.
if(NOT BRLCAD_CONFIGEXE)
  find_program(BRLCAD_CONFIGEXE brlcad-config)
endif(NOT BRLCAD_CONFIGEXE)
if(BRLCAD_CONFIGEXE)
  execute_process(COMMAND ${BRLCAD_CONFIGEXE} --version OUTPUT_VARIABLE BRLCAD_VERSION)
  string(STRIP ${BRLCAD_VERSION} BRLCAD_VERSION)
  if(BRLCAD_VERSION)
    string(REGEX REPLACE "([0-9]+)\\.[0-9]+\\.[0-9]+" "\\1" BRLCAD_MAJOR_VERSION "${BRLCAD_VERSION}")
    string(REGEX REPLACE "[0-9]+\\.([0-9]+)\\.[0-9]+" "\\1" BRLCAD_MINOR_VERSION "${BRLCAD_VERSION}")
    string(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" BRLCAD_PATCH_VERSION "${BRLCAD_VERSION}")
    set(BRLCAD_VERSION_FOUND TRUE)
  elseif(BRLCAD_VERSION)
    message(STATUS "\t\t'brlcad-config --version' was found and executed, but produced no output.")
    set(BRLCAD_VERSION_FOUND FALSE)
  endif(BRLCAD_VERSION)
else(BRLCAD_CONFIGEXE)
  message(STATUS "Could not locate 'brlcad-config' - no BRL-CAD version available")
  set(BRLCAD_VERSION_FOUND FALSE)
endif(BRLCAD_CONFIGEXE)

#TODO need to make the BRLCAD version checking a requirement for coreInterface, but nothing else.
#TODO figure out why brlcad-config isn't present on Windows.
##########################################################################
# First, search for BRL-CAD's own libraries
set(BRL-CAD_LIBS_SEARCH_LIST
  analyze
  bn
  bu
  cursor
  dm
  fb
  fft
  gcv
  ged
  icv
  multispectral
  optical
  orle
  pkg
  render
  rt
  sysv
  tclcad
  termio
  wdb
  )

foreach(brl_lib ${BRL-CAD_LIBS_SEARCH_LIST})
  string(TOUPPER ${brl_lib} LIBCORE)
  find_library(BRLCAD_${LIBCORE}_LIBRARY NAMES ${brl_lib} lib${brl_lib} PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
  if(BRLCAD_${LIBCORE}_LIBRARY)
    set(BRLCAD_LIBRARIES ${BRLCAD_LIBRARIES} ${BRLCAD_${LIBCORE}_LIBRARY})
  else(BRLCAD_${LIBCORE}_LIBRARY)
    set(BRLCAD_LIBRARIES_NOTFOUND ${BRLCAD_LIBRARIES_NOTFOUND} ${brl_lib})
  endif(BRLCAD_${LIBCORE}_LIBRARY)
endforeach(brl_lib ${BRL-CAD_LIBS_SEARCH_LIST})

# Then, look for customized src/other libraries that we need
# local versions of

set(BRL-CAD_SRC_OTHER_REQUIRED
  exppp
  express
  stepcore
  stepdai
  stepeditor
  steputils
  openNURBS
  )

foreach(ext_lib ${BRL-CAD_LIBS_SEARCH_LIST})
  string(TOUPPER ${ext_lib} LIBCORE)
  find_library(BRLCAD_${LIBCORE}_LIBRARY NAMES ${ext_lib} lib${ext_lib} PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
  if(BRLCAD_${LIBCORE}_LIBRARY)
    set(BRLCAD_LIBRARIES ${BRLCAD_LIBRARIES} ${BRLCAD_${LIBCORE}_LIBRARY})
  else(BRLCAD_${LIBCORE}_LIBRARY)
    set(BRLCAD_LIBRARIES_NOTFOUND ${BRLCAD_LIBRARIES_NOTFOUND} ${ext_lib})
  endif(BRLCAD_${LIBCORE}_LIBRARY)
endforeach(ext_lib ${BRL-CAD_LIBS_SEARCH_LIST})

# Lastly, we need to check for local installs in the BRL-CAD install of
# libraries that might otherwise be present on the system - if they are
# found in the BRL-CAD install tree, use those versions instead of any
# system results by setting the variables a find_package result would
# produce.

#  zlib
find_library(ZLIB_LIBRARY NAMES z zlib zdll PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_path(ZLIB_INCLUDE_DIR zlib.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)
set(ZLIB_LIBRARIES ${ZLIB_LIBRARY})
set(ZLIB_INCLUDE_DIRS ${ZLIB_INCLUDE_DIR})

#  libregex
find_library(REGEX_LIBRARY NAMES regex libregex PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_path(REGEX_INCLUDE_DIR regex.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)

#  libpng
find_library(PNG_LIBRARY NAMES png libpng png14 libpng14 png14d libpng14d PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_path(PNG_PNG_INCLUDE_DIR png.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)
set(PNG_INCLUDE_DIR ${PNG_PNG_INCLUDE_DIR} ${ZLIB_INCLUDE_DIR} )
set(PNG_LIBRARIES ${PNG_LIBRARY} ${ZLIB_LIBRARY})

#  libtermlib
find_library(TERMLIB_LIBRARY NAMES termlib libtermlib PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)

#  utahrle
find_library(UTAHRLE_LIBRARY NAMES utahrle libutahrle PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_path(UTAHRLE_INCLUDE_DIR rle.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)

#  tcl/tk
find_program(TCL_TCLSH_EXECUTABLE NAMES tclsh tclsh85 tclsh8.5 tclsh-8.5 tclsh-85 PATHS ${BRLCAD_BIN_DIR} NO_SYSTEM_PATH)
set(TCL_TCLSH ${TCL_TCLSH_EXECUTABLE})
find_program(TCL_WISH_EXECUTABLE NAMES wish wish85 wish8.5 wish-8.5 wish-85 PATHS ${BRLCAD_BIN_DIR} NO_SYSTEM_PATH)
set(TK_WISH ${TCL_WISH_EXECUTABLE})
find_library(TCL_LIBRARY NAMES tcl tcl85 tcl8.5 tcl-8.5 tcl-85 PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_library(TCL_TK_LIBRARY NAMES tk tk85 tk8.5 tk-8.5 tk-85 PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
set(TCL_LIBRARIES ${TCL_LIBRARY} ${TCL_TK_LIBRARY})
set(TK_LIBRARY ${TCL_TK_LIBRARY})
get_filename_component(TCL_CONF_PREFIX ${TCL_LIBRARY} PATH)
set(TK_CONF_PREFIX ${TCL_CONF_PREFIX})
set(TCL_TK_CONF_PREFIX ${TCL_CONF_PREFIX})
find_library(TCL_STUB_LIBRARY NAMES tclstub tclstub85 tclstub8.5 tclstub-8.5 tclstub-85 PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_library(TCL_TK_STUB_LIBRARY NAMES tkstub tkstub85 tkstub8.5 tkstub-8.5 tkstub-85 PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
set(TCL_STUB_LIBRARIES ${TCL_STUB_LIBRARY} ${TCL_TK_STUB_LIBRARY})
set(TK_STUB_LIBRARY ${TCL_TK_STUB_LIBRARY})
find_path(TCL_INCLUDE_PATH tcl.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)
find_path(TK_INCLUDE_PATH tk.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)
set(TCL_INCLUDE_DIRS ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

#  Togl
find_library(TOGL_LIBRARY NAMES togl PATHS ${BRLCAD_LIB_DIR} NO_SYSTEM_PATH)
find_path(TOGL_INCLUDE_DIR NAMES togl/togl.h PATHS ${BRLCAD_INCLUDE_DIRS} NO_SYSTEM_PATH)

##########################################################################
#Print status
if(BRLCAD_VERSION)
  message(STATUS "Found BRL-CAD ${BRLCAD_VERSION} at ${BRLCAD_BASE_DIR}")
else(BRLCAD_VERSION)
  message(STATUS "Found BRL-CAD at ${BRLCAD_BASE_DIR}")
endif(BRLCAD_VERSION)

#Set found flag - TODO: this is wrong, need to set this based on variables
set(BRLCAD_FOUND TRUE)

# Local Variables:
# tab-width: 8
# mode: cmake
# indent-tabs-mode: t
# End:
# ex: shiftwidth=2 tabstop=8
