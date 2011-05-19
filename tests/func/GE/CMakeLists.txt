
include_directories(
	${CPPUNIT_INCLUDE_DIR}
	${TCL_INCLUDE_DIRS}
	${BRLCAD_INCLUDE_DIRS}
)

SET( geTest_LIBS
	${BRLCAD_LIBRARIES}
	libgeomserv
)

add_executable(geTest GeometryEngineTest.cxx)
TARGET_LINK_LIBRARIES(geTest ${geTest_LIBS})