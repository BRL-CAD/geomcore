SET(server3_srcs
	main.cpp
	connection.cpp
	mime_types.cpp
	reply.cpp
	request_handler.cpp
	request_parser.cpp
	server.cpp
	)

add_executable(server3 ${server3_srcs})
target_link_libraries(server3 ${boost_thread_target} ${boost_system_target})

