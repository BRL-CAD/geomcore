include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
	${PROTOBUF_INCLUDE_DIRS}
	)

ADD_CUSTOM_COMMAND(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/stringdb.pb.cc ${CMAKE_CURRENT_BINARY_DIR}/stringdb.pb.h
	COMMAND protoc --proto_path=${CMAKE_CURRENT_SOURCE_DIR} --cpp_out=${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/stringdb.proto
	)

SET(client_srcs
	client.cpp
	${CMAKE_CURRENT_BINARY_DIR}/stringdb.pb.cc
	)

add_executable(client ${client_srcs})
target_link_libraries(client ${boost_system_target} libprotobuf ${ZLIB_LIBRARY})

