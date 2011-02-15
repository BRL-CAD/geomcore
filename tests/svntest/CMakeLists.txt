INCLUDE_DIRECTORIES(
	${BRLCAD_INCLUDE_DIRS}
)

SET(SVNTEST_SRCS
	main.c
)

add_executable(svntest ${SVNTEST_SRCS})
target_link_libraries(svntest svn_client svn_repos)
install(TARGETS svntest RUNTIME DESTINATION bin)
