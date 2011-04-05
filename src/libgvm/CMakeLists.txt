INCLUDE_DIRECTORIES(
	${CMAKE_CURRENT_SOURCE_DIR}
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

SET(gvm_SRCS
	mem.c
	objects.c
	repo.c
)

IF(BUILD_SHARED_LIBS)
	add_library(libgvm SHARED ${gvm_SRCS})
	target_link_libraries(libgvm svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
	set_target_properties(libgvm PROPERTIES PREFIX "")
	install(TARGETS libgvm DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgvm-static STATIC ${gvm_SRCS})
	target_link_libraries(libgvm-static svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
	IF(NOT WIN32)
		set_target_properties(libgvm-static PROPERTIES PREFIX "")
		set_target_properties(libgvm-static PROPERTIES OUTPUT_NAME "libgvm")
	ELSE(NOT WIN32)
		set_target_properties(libgvm-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libgvm-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)
