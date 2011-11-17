include_directories(
	${CMAKE_BINARY_DIR}/src/other/uuid
	${CMAKE_CURRENT_SOURCE_DIR}/../other/boost
	)

ADD_SUBDIRECTORY(asio_protobuf_sample)

add_executable(gsclient gstestclient.c)
target_link_libraries(gsclient uuid)

add_executable(blocking_tcp_echo_client blocking_tcp_echo_client.cpp)
target_link_libraries(blocking_tcp_echo_client ${boost_system_target})
