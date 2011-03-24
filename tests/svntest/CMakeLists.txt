INCLUDE_DIRECTORIES(
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

SET(SVNTEST_SRCS
	main.c
)

add_executable(svnTest ${SVNTEST_SRCS})
target_link_libraries(svnTest svn_client svn_repos svn_ra ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
install(TARGETS svnTest RUNTIME DESTINATION bin)
