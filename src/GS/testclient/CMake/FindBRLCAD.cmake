#########################################################################
#
# BRL-CAD
#
# Copyright (c) 1997-2011 United States Government as represented by
# the U.S. Army Research Laboratory.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this file; see the file named COPYING for more
# information.
#
#########################################################################
# @file FindBRLCAD.cmake
#
#  Try to find brlcad libraries.
#  Once done, this will define:
#
#   BRLCAD_FOUND - system has BRL-CAD
#   BRLCAD_VERSION - the BRL-CAD version string
#   BRLCAD_INCLUDE_DIRS - the BRL-CAD include directories
#   BRLCAD_LIBRARIES - link these to use the BRL-CAD Libraries
#
#   BRLCAD_ANALYZE_LIBRARY - BRL-CAD Analysis library
#   BRLCAD_BG_LIBRARY - BRL-CAD Geometry Algorithms library
#   BRLCAD_BN_LIBRARY - BRL-CAD Numerical library
#   BRLCAD_BREP_LIBRARY - BRL-CAD NURBS Brep Algorithms library
#   BRLCAD_BU_LIBRARY - BRL-CAD Utility library
#   BRLCAD_DM_LIBRARY - BRL-CAD Display Manager/Framebuffer library
#   BRLCAD_FFT_LIBRARY - BRL-CAD FFT library
#   BRLCAD_GCV_LIBRARY - BRL-CAD Geometry Conversion library
#   BRLCAD_GED_LIBRARY - BRL-CAD Geometry Editing library
#   BRLCAD_ICV_LIBRARY - BRL-CAD Image Conversion library
#   BRLCAD_OPTICAL_LIBRARY - BRL-CAD optical library
#   BRLCAD_PKG_LIBRARY - BRL-CAD libpkg
#   BRLCAD_RENDER_LIBRARY - librender
#   BRLCAD_RT_LIBRARY - BRL-CAD Raytracing library
#   BRLCAD_TCLCAD_LIBRARY - libtclcad
#   BRLCAD_WDB_LIBRARY - BRL-CAD Write Database library
#
#  Technically these are external but we need the versions
#  tweaked for BRL-CAD.  If a developer wishes to use the
#  BRL-CAD altered versions of these libraries to satisfy a
#  "generic" request for the library, they'll need to assign
#  the results of these variables to the non-BRL-CAD specific
#  variables they are using
#
#  BRLCAD_OPENNURBS_LIBRARY - openNURBS library
#
#########################################################################

if (NOT DEFINED BRLCAD_ROOT)
  set(BRLCAD_ROOT "$ENV{BRLCAD_ROOT}")
endif (NOT DEFINED BRLCAD_ROOT)

# First, find the install directories.
if(NOT BRLCAD_ROOT)
  # Try looking for BRL-CAD's brlcad-config - it should give us
  # a location for bin, and the parent will be the root dir
  if(NOT BRLCAD_ROOT)
    find_program(BRLCAD_CONFIGEXE brlcad-config)
    if(BRLCAD_CONFIGEXE)
      execute_process(COMMAND brlcad-config --prefix OUTPUT_VARIABLE BRLCAD_ROOT)
    endif(BRLCAD_CONFIGEXE)
  endif(NOT BRLCAD_ROOT)

  # If that didn't work, see if we can find brlcad-config and use the parent path
  if(NOT BRLCAD_ROOT)
    find_path(BRLCAD_BIN_DIR brlcad-config)
    if(BRLCAD_BIN_DIR)
      get_filename_component(BRLCAD_ROOT ${BRLCAD_BIN_DIR} PATH)
    endif(BRLCAD_BIN_DIR)
  endif(NOT BRLCAD_ROOT)

  # Look for headers if we come up empty with brlcad-config
  if(NOT BRLCAD_ROOT)
    file(GLOB CAD_DIRS LIST_DIRECTORIES TRUE "/usr/brlcad/*")
    foreach(CDIR ${CAD_DIRS})
      if (IS_DIRECTORY ${CDIR})
	list(APPEND BRLCAD_HEADERS_DIR_CANDIDATES "${CDIR}/include/brlcad")
      endif (IS_DIRECTORY ${CDIR})
    endforeach(CDIR ${CAD_DIRS})
    file(GLOB CAD_DIRS_LOCAL LIST_DIRECTORIES TRUE "/usr/local/brlcad/*")
    foreach(CDIR ${CAD_DIRS_LOCAL})
      if (IS_DIRECTORY ${CDIR})
	list(APPEND BRLCAD_HEADERS_DIR_CANDIDATES "${CDIR}/include/brlcad")
      endif (IS_DIRECTORY ${CDIR})
    endforeach(CDIR ${CAD_DIRS_LOCAL})
    list(SORT BRLCAD_HEADERS_DIR_CANDIDATES COMPARE NATURAL ORDER DESCENDING)
    foreach(CDIR ${BRLCAD_HEADERS_DIR_CANDIDATES})
      if (NOT BRLCAD_ROOT)
	find_path(BRLCAD_HEADERS_DIR NAMES bu.h bn.h rt.h PATHS ${CDIR})
	if(BRLCAD_HEADERS_DIR)
	  get_filename_component(BRLCAD_INC_DIR ${BRLCAD_HEADERS_DIR} PATH)
	  get_filename_component(BRLCAD_ROOT ${BRLCAD_INC_DIR} PATH)
	endif(BRLCAD_HEADERS_DIR)
      endif (NOT BRLCAD_ROOT)
    endforeach(CDIR ${BRLCAD_HEADERS_DIR_CANDIDATES})
  endif(NOT BRLCAD_ROOT)

  if(NOT BRLCAD_ROOT)
    message(FATAL_ERROR "\nCould not find BRL-CAD root directory - please set BRLCAD_ROOT in CMake")
  endif(NOT BRLCAD_ROOT)
endif(NOT BRLCAD_ROOT)

#Find include directories
if(NOT BRLCAD_HEADERS_DIR)
  find_path(BRLCAD_HEADERS_DIR NAMES bu.h HINTS ${BRLCAD_ROOT} PATH_SUFFIXES include/brlcad)
  get_filename_component(BRLCAD_HEADERS_PARENT_DIR ${BRLCAD_HEADERS_DIR} PATH)
endif(NOT BRLCAD_HEADERS_DIR)
find_path(BRLCAD_OPENNURBS_HEADERS_DIR NAMES opennurbs.h HINTS ${BRLCAD_ROOT} PATH_SUFFIXES include/openNURBS include/opennurbs)
set(BRLCAD_INCLUDE_DIRS ${BRLCAD_HEADERS_PARENT_DIR} ${BRLCAD_HEADERS_DIR} ${BRLCAD_OPENNURBS_HEADERS_DIR})
set(BRLCAD_INCLUDE_DIRS ${BRLCAD_INCLUDE_DIRS} CACHE STRING "BRL-CAD include directories")

#Find library directory
find_path(BRLCAD_LIB_DIR "libbu${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS ${BRLCAD_ROOT} PATH_SUFFIXES lib libs)

#Find binary directory
if(NOT BRLCAD_BIN_DIR)
  find_path(BRLCAD_BIN_DIR brlcad-config PATHS ${BRLCAD_ROOT} PATH_SUFFIXES bin)
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

##########################################################################
# First, search for BRL-CAD's own libraries
set(BRL-CAD_LIBS_SEARCH_LIST
  analyze
  bg
  bn
  brep
  bu
  dm
  fft
  gcv
  ged
  icv
  nmg
  optical
  pkg
  render
  rt
  tclcad
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

##########################################################################
#Print status
if(BRLCAD_VERSION)
  message(STATUS "Found BRL-CAD ${BRLCAD_VERSION} at ${BRLCAD_ROOT}")
else(BRLCAD_VERSION)
  message(STATUS "Found BRL-CAD at ${BRLCAD_ROOT}")
endif(BRLCAD_VERSION)

#Set found flag - TODO: this is wrong, need to set this based on variables
set(BRLCAD_FOUND TRUE)

# Local Variables:
# tab-width: 8
# mode: cmake
# indent-tabs-mode: t
# End:
# ex: shiftwidth=2 tabstop=8
