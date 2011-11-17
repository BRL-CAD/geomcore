include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}
	${ZLIB_INCLUDE_DIR}
	)

#libprotobuf_lite_la_LDFLAGS = -version-info 7:0:0 -export-dynamic -no-undefined
SET(protobuf_lite_SOURCES                                  
	google/protobuf/stubs/common.cc                              
	google/protobuf/stubs/once.cc                                
	google/protobuf/stubs/hash.h                                 
	google/protobuf/stubs/map-util.h                             
	google/protobuf/stubs/stl_util-inl.h                         
	google/protobuf/extension_set.cc                             
	google/protobuf/generated_message_util.cc                    
	google/protobuf/message_lite.cc                              
	google/protobuf/repeated_field.cc                            
	google/protobuf/wire_format_lite.cc                          
	google/protobuf/io/coded_stream.cc                           
	google/protobuf/io/coded_stream_inl.h                        
	google/protobuf/io/zero_copy_stream.cc                       
	google/protobuf/io/zero_copy_stream_impl_lite.cc
	)
add_library(libprotobuf_lite SHARED ${protobuf_lite_SOURCES})
target_link_libraries(libprotobuf_lite ${CMAKE_THREAD_LIBS_INIT})
set_target_properties(libprotobuf_lite PROPERTIES PREFIX "")

#libprotobuf_la_LDFLAGS = -version-info 7:0:0 -export-dynamic -no-undefined
SET(protobuf_SOURCES
	${protobuf_lite_SOURCES}                               
	google/protobuf/stubs/strutil.cc                             
	google/protobuf/stubs/strutil.h                              
	google/protobuf/stubs/substitute.cc                          
	google/protobuf/stubs/substitute.h                           
	google/protobuf/stubs/structurally_valid.cc                  
	google/protobuf/descriptor.cc                                
	google/protobuf/descriptor.pb.cc                             
	google/protobuf/descriptor_database.cc                       
	google/protobuf/dynamic_message.cc                           
	google/protobuf/extension_set_heavy.cc                       
	google/protobuf/generated_message_reflection.cc              
	google/protobuf/message.cc                                   
	google/protobuf/reflection_ops.cc                            
	google/protobuf/service.cc                                   
	google/protobuf/text_format.cc                               
	google/protobuf/unknown_field_set.cc                         
	google/protobuf/wire_format.cc                               
	google/protobuf/io/gzip_stream.cc                            
	google/protobuf/io/printer.cc                                
	google/protobuf/io/tokenizer.cc                              
	google/protobuf/io/zero_copy_stream_impl.cc                  
	google/protobuf/compiler/importer.cc                         
	google/protobuf/compiler/parser.cc
	)
add_library(libprotobuf SHARED ${protobuf_SOURCES})
target_link_libraries(libprotobuf ${CMAKE_THREAD_LIBS_INIT})
set_target_properties(libprotobuf PROPERTIES PREFIX "")

#libprotoc_la_LDFLAGS = -version-info 7:0:0 -export-dynamic -no-undefined
SET(protoc_SOURCES
  google/protobuf/compiler/code_generator.cc                   
  google/protobuf/compiler/command_line_interface.cc           
  google/protobuf/compiler/plugin.cc                           
  google/protobuf/compiler/plugin.pb.cc                        
  google/protobuf/compiler/subprocess.cc                       
  google/protobuf/compiler/subprocess.h                        
  google/protobuf/compiler/zip_writer.cc                       
  google/protobuf/compiler/zip_writer.h                        
  google/protobuf/compiler/cpp/cpp_enum.cc                     
  google/protobuf/compiler/cpp/cpp_enum.h                      
  google/protobuf/compiler/cpp/cpp_enum_field.cc               
  google/protobuf/compiler/cpp/cpp_enum_field.h                
  google/protobuf/compiler/cpp/cpp_extension.cc                
  google/protobuf/compiler/cpp/cpp_extension.h                 
  google/protobuf/compiler/cpp/cpp_field.cc                    
  google/protobuf/compiler/cpp/cpp_field.h                     
  google/protobuf/compiler/cpp/cpp_file.cc                     
  google/protobuf/compiler/cpp/cpp_file.h                      
  google/protobuf/compiler/cpp/cpp_generator.cc                
  google/protobuf/compiler/cpp/cpp_helpers.cc                  
  google/protobuf/compiler/cpp/cpp_helpers.h                   
  google/protobuf/compiler/cpp/cpp_message.cc                  
  google/protobuf/compiler/cpp/cpp_message.h                   
  google/protobuf/compiler/cpp/cpp_message_field.cc            
  google/protobuf/compiler/cpp/cpp_message_field.h             
  google/protobuf/compiler/cpp/cpp_primitive_field.cc          
  google/protobuf/compiler/cpp/cpp_primitive_field.h           
  google/protobuf/compiler/cpp/cpp_service.cc                  
  google/protobuf/compiler/cpp/cpp_service.h                   
  google/protobuf/compiler/cpp/cpp_string_field.cc             
  google/protobuf/compiler/cpp/cpp_string_field.h              
  google/protobuf/compiler/java/java_enum.cc                   
  google/protobuf/compiler/java/java_enum.h                    
  google/protobuf/compiler/java/java_enum_field.cc             
  google/protobuf/compiler/java/java_enum_field.h              
  google/protobuf/compiler/java/java_extension.cc              
  google/protobuf/compiler/java/java_extension.h               
  google/protobuf/compiler/java/java_field.cc                  
  google/protobuf/compiler/java/java_field.h                   
  google/protobuf/compiler/java/java_file.cc                   
  google/protobuf/compiler/java/java_file.h                    
  google/protobuf/compiler/java/java_generator.cc              
  google/protobuf/compiler/java/java_helpers.cc                
  google/protobuf/compiler/java/java_helpers.h                 
  google/protobuf/compiler/java/java_message.cc                
  google/protobuf/compiler/java/java_message.h                 
  google/protobuf/compiler/java/java_message_field.cc          
  google/protobuf/compiler/java/java_message_field.h           
  google/protobuf/compiler/java/java_primitive_field.cc        
  google/protobuf/compiler/java/java_primitive_field.h         
  google/protobuf/compiler/java/java_service.cc                
  google/protobuf/compiler/java/java_service.h                 
  google/protobuf/compiler/java/java_string_field.cc           
  google/protobuf/compiler/java/java_string_field.h            
  google/protobuf/compiler/python/python_generator.cc
  )
add_library(libprotoc SHARED ${protoc_SOURCES})
target_link_libraries(libprotoc ${CMAKE_THREAD_LIBS_INIT} libprotobuf)
set_target_properties(libprotoc PROPERTIES PREFIX "")

add_executable(protoc google/protobuf/compiler/main.cc)
target_link_libraries(protoc ${CMAKE_THREAD_LIBS_INIT} libprotobuf libprotoc ${ZLIB_LIBRARY})

IF(ZLIB_FOUND)
	add_executable(zcgzip google/protobuf/testing/zcgunzip.cc)
	target_link_libraries(zcgzip libprotobuf ${ZLIB_LIBRARY})
	add_executable(zcgunzip google/protobuf/testing/zcgunzip.cc)
	target_link_libraries(zcgunzip libprotobuf ${ZLIB_LIBRARY})
	SET(GZHEADERS google/protobuf/io/gzip_stream.h)
ENDIF(ZLIB_FOUND)

# To allow for re-generation of the .pb.cc and .pb.h files from proto
# sources, the following custom commands are added.
ADD_CUSTOM_COMMAND(
	OUTPUT ${CMAKE_BINARY_DIR}/google/protobuf/descriptor.pb.cc ${CMAKE_BINARY_DIR}/google/protobuf/descriptor.pb.h
	COMMAND protoc --proto_path=${CMAKE_CURRENT_SOURCE_DIR} --cpp_out=dllexport_decl=LIBPROTOBUF_EXPORT:. ${CMAKE_CURRENT_SOURCE_DIR}/google/protobuf/descriptor.proto
	DEPENDS protoc ${CMAKE_CURRENT_SOURCE_DIR}/google/protobuf/descriptor.proto
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	)
ADD_CUSTOM_COMMAND(
	OUTPUT ${CMAKE_BINARY_DIR}/google/protobuf/compiler/plugin.pb.cc ${CMAKE_BINARY_DIR}/google/protobuf/compiler/plugin.pb.h
	COMMAND protoc --proto_path=${CMAKE_CURRENT_SOURCE_DIR} --cpp_out=dllexport_decl=LIBPROTOBUF_EXPORT:. ${CMAKE_CURRENT_SOURCE_DIR}/google/protobuf/compiler/plugin.proto
	DEPENDS protoc	${CMAKE_CURRENT_SOURCE_DIR}/google/protobuf/compiler/plugin.proto
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	)

ADD_CUSTOM_TARGET(proto_srcs DEPENDS
	${CMAKE_BINARY_DIR}/google/protobuf/descriptor.pb.cc
	${CMAKE_BINARY_DIR}/google/protobuf/descriptor.pb.h
	${CMAKE_BINARY_DIR}/google/protobuf/compiler/plugin.pb.cc
	${CMAKE_BINARY_DIR}/google/protobuf/compiler/plugin.pb.h
	)

# If I say "dist_include_DATA", automake complains that $(includedir) is not
# a "legitimate" directory for DATA.  Screw you, automake.
#protodir = $(includedir)
#nobase_dist_proto_DATA = google/protobuf/descriptor.proto \
#                         google/protobuf/compiler/plugin.proto

SET(include_HEADERS                                       
  google/protobuf/stubs/common.h                               
  google/protobuf/stubs/once.h                                 
  google/protobuf/descriptor.h                                 
  google/protobuf/descriptor.pb.h                              
  google/protobuf/descriptor_database.h                        
  google/protobuf/dynamic_message.h                            
  google/protobuf/extension_set.h                              
  google/protobuf/generated_message_util.h                     
  google/protobuf/generated_message_reflection.h               
  google/protobuf/message.h                                    
  google/protobuf/message_lite.h                               
  google/protobuf/reflection_ops.h                             
  google/protobuf/repeated_field.h                             
  google/protobuf/service.h                                    
  google/protobuf/text_format.h                                
  google/protobuf/unknown_field_set.h                          
  google/protobuf/wire_format.h                                
  google/protobuf/wire_format_lite.h                           
  google/protobuf/wire_format_lite_inl.h                       
  google/protobuf/io/coded_stream.h                            
  ${GZHEADERS}                                                 
  google/protobuf/io/printer.h                                 
  google/protobuf/io/tokenizer.h                               
  google/protobuf/io/zero_copy_stream.h                        
  google/protobuf/io/zero_copy_stream_impl.h                   
  google/protobuf/io/zero_copy_stream_impl_lite.h              
  google/protobuf/compiler/code_generator.h                    
  google/protobuf/compiler/command_line_interface.h            
  google/protobuf/compiler/importer.h                          
  google/protobuf/compiler/parser.h                            
  google/protobuf/compiler/plugin.h                            
  google/protobuf/compiler/plugin.pb.h                         
  google/protobuf/compiler/cpp/cpp_generator.h                 
  google/protobuf/compiler/java/java_generator.h               
  google/protobuf/compiler/python/python_generator.h
  )
