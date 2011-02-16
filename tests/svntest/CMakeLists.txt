INCLUDE_DIRECTORIES(
	${BRLCAD_INCLUDE_DIRS}
)

SET(SVNTEST_SRCS
	main.c
)

add_executable(svntest ${SVNTEST_SRCS})
target_link_libraries(svntest svn_client svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
install(TARGETS svntest RUNTIME DESTINATION bin)
