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
#
# - Find CppUnit
# This module finds an installed CppUnit package.
#
# It sets the following variables:
#  CPPUNIT_FOUND       - Set to false, or undefined, if CppUnit isn't found.
#  CPPUNIT_INCLUDE_DIR - The CppUnit include directory.
#  CPPUNIT_LIBRARY     - The CppUnit library to link against.
#
#########################################################################

FIND_PATH(CPPUNIT_INCLUDE_DIR cppunit/Test.h)
FIND_LIBRARY(CPPUNIT_LIBRARY NAMES cppunit)

MARK_AS_ADVANCED(CPPUNIT_INCLUDE_DIR CPPUNIT_LIBRARY)

# handle the QUIETLY and REQUIRED arguments and set CPPUNIT_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CPPUNIT DEFAULT_MSG CPPUNIT_INCLUDE_DIR CPPUNIT_LIBRARY)

