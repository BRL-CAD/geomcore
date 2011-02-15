#########################################################################
#
#	BRL-CAD
#
#	Copyright (c) 2011 United States Government as represented by
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
#	@file rt^3/src/coreInterface/CMakeLists.txt
#
#	Origin -
#		TNO (Netherlands)
#		IABG mbH (Germany)
#
#########################################################################

RT3_PROJECT(coreinterface)

IF(BRLCAD_VERSION_FOUND)

	SET(BRLCADVERSION_H_CONTENT
		"#define BRLCAD_LIB_MAJOR ${BRLCAD_MAJOR_VERSION}\n"
		"#define BRLCAD_LIB_MINOR ${BRLCAD_MINOR_VERSION}\n"
		"#define BRLCAD_LIB_PATCH ${BRLCAD_PATCH_VERSION}\n"
	)
	FILE(WRITE ${RT3_PUBLIC_HEADER_DIR}/brlcad/brlcadversion.h ${BRLCADVERSION_H_CONTENT})
ENDIF(BRLCAD_VERSION_FOUND)


if(EXISTS ${RT3_PUBLIC_HEADER_DIR}/brlcad/brlcadversion.h)
	#Set Include Dirs
	RT3_PROJECT_ADD_INCLUDE_DIRS(
	    ${TCL_INCLUDE_DIRS}
		 ${BRLCAD_INCLUDE_DIRS}
	)

	#Set Libs
	RT3_PROJECT_ADD_LIBS(
	    ${BRLCAD_LIBRARIES}
	)

	#set Source files
	RT3_PROJECT_ADD_SOURCES (
		Arb8.cpp
		Combination.cpp
		Cone.cpp
		ConstDatabase.cpp
		Database.cpp
		Ellipsoid.cpp
		EllipticalTorus.cpp
		FileDatabase.cpp
		globals.cpp
		Halfspace.cpp
		HyperbolicCylinder.cpp
		Hyperboloid.cpp
		MemoryDatabase.cpp
		Object.cpp
		ParabolicCylinder.cpp
		Paraboloid.cpp
		Particle.cpp
		Torus.cpp
		Unknown.cpp
	)

	#Temporary fix fo the types.h vs unistd.h vs stdint.h vs pstdint.h declaration conflicts
	#SET_SOURCE_FILES_PROPERTIES(${LIBCOREINTERFACE_SOURCES} PROPERTIES COMPILE_FLAGS "-DHAVE_STDINT_H")

	#Build the project
	RT3_PROJECT_BUILD_LIB()
ELSE(EXISTS ${RT3_PUBLIC_HEADER_DIR}/brlcad/brlcadversion.h)
	MESSAGE(STATUS "\tConfiguration for '${PROJECT_NAME}'...  Omitted.")
ENDIF(EXISTS ${RT3_PUBLIC_HEADER_DIR}/brlcad/brlcadversion.h)
