include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/../other/boost
	)
ADD_SUBDIRECTORY(asio_protobuf_sample)
ADD_SUBDIRECTORY(server3)
add_executable(async_tcp_echo_server async_tcp_echo_server.cpp)
target_link_libraries(async_tcp_echo_server ${boost_system_target})
