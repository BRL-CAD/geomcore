INCLUDE_DIRECTORIES(
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

SET(geomsvn_SRCS
	breakout.c
	commit.c
	diff.c
	assemble.c
)

IF(BUILD_SHARED_LIBS)
	add_library(libgeomsvn SHARED ${geomsvn_SRCS})
	target_link_libraries(libgeomsvn svn_client svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
	set_target_properties(libgeomsvn PROPERTIES PREFIX "")
	install(TARGETS libgeomsvn DESTINATION lib)
ENDIF(BUILD_SHARED_LIBS)
IF(BUILD_STATIC_LIBS)
	add_library(libgeomsvn-static STATIC ${geomsvn_SRCS})
	target_link_libraries(libgeomsvn-static svn_client svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
	IF(NOT WIN32)
		set_target_properties(libgeomsvn-static PROPERTIES PREFIX "")
		set_target_properties(libgeomsvn-static PROPERTIES OUTPUT_NAME "libgeomsvn")
	ELSE(NOT WIN32)
		set_target_properties(libgeomsvn-static PROPERTIES PREFIX "lib")
	ENDIF(NOT WIN32)
	install(TARGETS libgeomsvn-static DESTINATION lib)
ENDIF(BUILD_STATIC_LIBS)
