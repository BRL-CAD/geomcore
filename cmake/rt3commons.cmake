SET (RT3_FILE_HEADER "/*                      
 * BRL-CAD
 *
 * Copyright (c) 2010 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
")

SET (RT3_FILE_AUTOGEN_STATEMENT "   
 /******************************************************************/
 /******* THIS FILE WAS AUTO GENERATED BY RT3's BUILD SYSTEM *******/
 /************************** DO NOT EDIT! **************************/
 /******************************************************************/
")
 


SET (RT3_FILE_FOOTER "// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
")



#
# RT3_PROJECT stuff.
#

MACRO(RT3_PROJECT PROJNAME)
    PROJECT(${PROJNAME})
    
    STRING(TOUPPER ${PROJECT_NAME} NAME_UPPER)
    
    #Make a var that points to the dir where the lib header is
    SET(${PROJECT_NAME}_PROJECT_DIR ${PROJECT_SOURCE_DIR} CACHE INTERNAL "")
    SET(${NAME_UPPER}_PROJECT_DIR ${PROJECT_SOURCE_DIR} CACHE INTERNAL "")    
    #MESSAGE(STATUS "${PROJECT_NAME}_PROJECT_DIR = ${${PROJECT_NAME}_PROJECT_DIR}")
    #MESSAGE(STATUS "${NAME_UPPER}_PROJECT_DIR = ${${NAME_UPPER}_PROJECT_DIR}")
        
    SET(${NAME_UPPER}_INCLUDE_DIRS ${GLOBAL_INCLUDE_DIRS} CACHE INTERNAL "") #Global included by default
    SET(${NAME_UPPER}_LINK_LIBS CACHE INTERNAL "")

    SET(${NAME_UPPER}_SOURCES CACHE INTERNAL "")
    SET(${NAME_UPPER}_INST_HEADERS CACHE INTERNAL "")
    SET(${NAME_UPPER}_NOINST_HEADERS CACHE INTERNAL "")
    SET(${NAME_UPPER}_QT_INST_HEADERS CACHE INTERNAL "")
    SET(${NAME_UPPER}_QT_NOINST_HEADERS CACHE INTERNAL "")
    SET(${NAME_UPPER}_MOCCED_INST_HEADERS CACHE INTERNAL "")
    SET(${NAME_UPPER}_MOCCED_NOINST_HEADERS CACHE INTERNAL "")

    SET(${NAME_UPPER}_ALL_INST_HEADERS CACHE INTERNAL "")
ENDMACRO(RT3_PROJECT FILENAME)

##Macros for building RT3_PROJECT variables
MACRO(RT3_PROJECT_ADD VARNAME)
    STRING(TOUPPER ${PROJECT_NAME} NAME_UPPER)
 
    FOREACH (tFile ${ARGN})
        SET(${NAME_UPPER}_${VARNAME} ${${NAME_UPPER}_${VARNAME}} ${tFile} CACHE INTERNAL "")
    ENDFOREACH (tFile)
ENDMACRO(RT3_PROJECT_ADD)

MACRO(RT3_PROJECT_ADD_INCLUDE_DIRS)    
        RT3_PROJECT_ADD("INCLUDE_DIRS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_INCLUDE_DIRS)

MACRO(RT3_PROJECT_ADD_LIBS)    
        RT3_PROJECT_ADD("LINK_LIBS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_LIBS)

MACRO(RT3_PROJECT_ADD_SOURCES)    
        RT3_PROJECT_ADD("SOURCES" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_SOURCES)

MACRO(RT3_PROJECT_ADD_INST_HEADERS)    
        RT3_PROJECT_ADD("INST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_INST_HEADERS)

MACRO(RT3_PROJECT_ADD_NOINST_HEADERS)    
        RT3_PROJECT_ADD("NOINST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_NOINST_HEADERS)

MACRO(RT3_PROJECT_ADD_QT_INST_HEADERS)    
        RT3_PROJECT_ADD("QT_INST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_QT_INST_HEADERS)

MACRO(RT3_PROJECT_ADD_QT_NOINST_HEADERS)    
        RT3_PROJECT_ADD("QT_NOINST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_QT_NOINST_HEADERS)

MACRO(RT3_PROJECT_ADD_MOCCED_INST_HEADERS)    
        RT3_PROJECT_ADD("MOCCED_INST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_MOCCED_INST_HEADERS)

MACRO(RT3_PROJECT_ADD_MOCCED_NOINST_HEADERS)    
        RT3_PROJECT_ADD("MOCCED_NOINST_HEADERS" ${ARGN})
ENDMACRO(RT3_PROJECT_ADD_MOCCED_NOINST_HEADERS)


##Macro for writing the library's public header file
MACRO(MAKE_LIBRARY_HEADER_FILE)

    SET(FILENAME "${PROJECT_NAME}.h")
    STRING (TOUPPER ${FILENAME} FILENAME_UPPER)
    
    #Build the __include__ name    
    STRING (REPLACE "." "_" INCLUDE_NAME ${FILENAME_UPPER})
    SET(INCLUDE_NAME "__${INCLUDE_NAME}__")
        
    SET(PATH_AND_NAME ${RT3_PUBLIC_HEADER_DIR}/${FILENAME})
        
    FILE(WRITE ${PATH_AND_NAME} ${RT3_FILE_HEADER})
    FILE(APPEND ${PATH_AND_NAME} ${RT3_FILE_AUTOGEN_STATEMENT})
    FILE(APPEND ${PATH_AND_NAME} "\n")
    FILE(APPEND ${PATH_AND_NAME} "#ifndef ${INCLUDE_NAME}\n")
    FILE(APPEND ${PATH_AND_NAME} "#define ${INCLUDE_NAME}\n")
    FILE(APPEND ${PATH_AND_NAME} "\n")
 
    FOREACH (headerFile ${${NAME_UPPER}_ALL_INST_HEADERS})
        FILE(APPEND ${PATH_AND_NAME} "#include \"${headerFile}\"\n")
        
        IF (NOT EXISTS ${RT3_PUBLIC_HEADER_DIR}/${headerFile})
            SET(ERROR_MSG "Compiled Lib Header (${FILENAME}) points to file /include/${headerFile}, which could not be found.")

            SET (RT3_COMPILE_WARNINGS ON CACHE INTERNAL "")
            SET (RT3_LAST_WARNING_MSG "Last Error: ${ERROR_MSG}" CACHE INTERNAL "")
            MESSAGE(WARNING "!!  ${ERROR_MSG}")
        ENDIF (NOT EXISTS ${RT3_PUBLIC_HEADER_DIR}/${headerFile})
        
    ENDFOREACH (headerFile)
 
    FILE(APPEND ${PATH_AND_NAME} "\n")
    FILE(APPEND ${PATH_AND_NAME} "#endif /* ${INCLUDE_NAME} */\n\n")
    FILE(APPEND ${PATH_AND_NAME} ${RT3_FILE_FOOTER})
ENDMACRO(MAKE_LIBRARY_HEADER_FILE)

##Macro to print out the project's config
MACRO(RT3_PROJECT_PRINT)
    STRING(TOUPPER ${PROJECT_NAME} NAME_UPPER)
    
    IF(RT3_VERBOSE_CMAKE_OUTPUT)        
        MESSAGE(STATUS "\tConfiguration for '${PROJECT_NAME}':")
    
        IF(${NAME_UPPER}_INCLUDE_DIRS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' include dirs:  ${${NAME_UPPER}_INCLUDE_DIRS}")
        ELSE(${NAME_UPPER}_INCLUDE_DIRS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no include dirs.")
        ENDIF(${NAME_UPPER}_INCLUDE_DIRS)
        
        IF(${NAME_UPPER}_LINK_LIBS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' linked libraries:   ${${NAME_UPPER}_LINK_LIBS}")
        ELSE(${NAME_UPPER}_LINK_LIBS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no linked libraries.")
        ENDIF(${NAME_UPPER}_LINK_LIBS)
     
        IF(${NAME_UPPER}_SOURCES)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' source files:   ${${NAME_UPPER}_SOURCES}")
        ELSE(${NAME_UPPER}_SOURCES)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no source files!")
        ENDIF(${NAME_UPPER}_SOURCES)

        IF(${NAME_UPPER}_INST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' installable header files:   ${${NAME_UPPER}_INST_HEADERS}")
        ELSE(${NAME_UPPER}_INST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no installable header files!")
        ENDIF(${NAME_UPPER}_INST_HEADERS)

        IF(${NAME_UPPER}_NOINST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' non-installable header files:   ${${NAME_UPPER}_NOINST_HEADERS}")
        ELSE(${NAME_UPPER}_NOINST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no non-installable header files!")
        ENDIF(${NAME_UPPER}_NOINST_HEADERS)

        IF(${NAME_UPPER}_QT_INST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' installable QT header files:   ${${NAME_UPPER}_QT_INST_HEADERS}")
        ELSE(${NAME_UPPER}_QT_INST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no installable QT header files!")
        ENDIF(${NAME_UPPER}_QT_INST_HEADERS)

        IF(${NAME_UPPER}_QT_NOINST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' non-installable QT header files:   ${${NAME_UPPER}_QT_NOINST_HEADERS}")
        ELSE(${NAME_UPPER}_QT_NOINST_HEADERS)
            MESSAGE(STATUS "\t\t'${PROJECT_NAME}' has no non-installable QT header files!")
        ENDIF(${NAME_UPPER}_QT_NOINST_HEADERS)

    ENDIF(RT3_VERBOSE_CMAKE_OUTPUT)
    
ENDMACRO(RT3_PROJECT_PRINT)

##Macro to build the project as a library
MACRO(RT3_PROJECT_BUILD_LIB)
    STRING(TOUPPER ${PROJECT_NAME} NAME_UPPER)
    INCLUDE_DIRECTORIES(${${NAME_UPPER}_INCLUDE_DIRS})

    MESSAGE (STATUS "${NAME_UPPER}_QT_INST_HEADERS: ${${NAME_UPPER}_QT_INST_HEADERS}")
    MESSAGE (STATUS "${NAME_UPPER}_QT_NOINST_HEADERS: ${${NAME_UPPER}_QT_NOINST_HEADERS}")

    #Do the Moccing for NOINST QT headers
    FOREACH (tFile ${${NAME_UPPER}_QT_NOINST_HEADERS})
        SET (tMoc "")
        qt4_wrap_cpp(tMoc ${tFile})
        RT3_PROJECT_ADD_MOCCED_NOINST_HEADERS(${tMoc})
    ENDFOREACH (tFile)
    
    #Do the Moccing for INST QT headers
    FOREACH (tFile ${${NAME_UPPER}_QT_INST_HEADERS})
        SET (tMoc "")
        qt4_wrap_cpp(tMoc ${RT3_ROOT}/include/${tFile})
        RT3_PROJECT_ADD_MOCCED_INST_HEADERS(${tMoc})        
    ENDFOREACH (tFile)

    SET(${NAME_UPPER}_ALL_INST_HEADERS ${${NAME_UPPER}_QT_INST_HEADERS} ${${NAME_UPPER}_INST_HEADERS} CACHE INTERNAL "")

    MAKE_LIBRARY_HEADER_FILE()
    RT3_PROJECT_PRINT()
        
    ADD_LIBRARY (${PROJECT_NAME} SHARED ${${NAME_UPPER}_SOURCES} ${${NAME_UPPER}_MOCCED_INST_HEADERS}  ${${NAME_UPPER}_MOCCED_NOINST_HEADERS})
    TARGET_LINK_LIBRARIES(${PROJECT_NAME}  ${${NAME_UPPER}_LINK_LIBS})
    
    IF(RT3_VERBOSE_CMAKE_OUTPUT)        
        MESSAGE(STATUS "\tDone.")
    ELSE (RT3_VERBOSE_CMAKE_OUTPUT)        
        MESSAGE(STATUS "\tConfiguration for '${PROJECT_NAME}'... Done.")
    ENDIF(RT3_VERBOSE_CMAKE_OUTPUT)  
ENDMACRO(RT3_PROJECT_BUILD_LIB)

##Macro to build the project as an Executable
MACRO(RT3_PROJECT_BUILD_EXE)
    STRING(TOUPPER ${PROJECT_NAME} NAME_UPPER)
    INCLUDE_DIRECTORIES(${${NAME_UPPER}_INCLUDE_DIRS})
    
    #Do the Moccing for NOINST QT headers
    FOREACH (tFile ${${NAME_UPPER}_QT_NOINST_HEADERS})
        SET (tMoc "")
        qt4_wrap_cpp(tMoc ${tFile})
        RT3_PROJECT_ADD_MOCCED_NOINST_HEADERS(${tMoc})
    ENDFOREACH (tFile)
    
    #Do the Moccing for INST QT headers
    FOREACH (tFile ${${NAME_UPPER}_QT_INST_HEADERS})
        SET (tMoc "")
        qt4_wrap_cpp(tMoc ${RT3_ROOT}/include/${tFile})
        RT3_PROJECT_ADD_MOCCED_INST_HEADERS(${tMoc})
    ENDFOREACH (tFile)

    SET(${NAME_UPPER}_ALL_INST_HEADERS ${${NAME_UPPER}_INST_HEADERS} ${${NAME_UPPER}_QT_INST_HEADERS})

    RT3_PROJECT_PRINT()
    
    ADD_EXECUTABLE (${PROJECT_NAME} ${${NAME_UPPER}_SOURCES} ${${NAME_UPPER}_MOCCED_INST_HEADERS}  ${${NAME_UPPER}_MOCCED_NOINST_HEADERS})
    TARGET_LINK_LIBRARIES(${PROJECT_NAME}  ${${NAME_UPPER}_LINK_LIBS})

    IF(RT3_VERBOSE_CMAKE_OUTPUT)        
        MESSAGE(STATUS "\tDone.")
    ELSE (RT3_VERBOSE_CMAKE_OUTPUT)        
        MESSAGE(STATUS "\tConfiguration for '${PROJECT_NAME}'... Done.")
    ENDIF(RT3_VERBOSE_CMAKE_OUTPUT)        
ENDMACRO(RT3_PROJECT_BUILD_EXE)











