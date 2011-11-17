# Do the same tests the protobuf m4 macro does for hash functions

IF(CMAKE_COMPILER_IS_GNUCXX)
EXEC_PROGRAM(${CMAKE_CXX_COMPILER}
	ARGS -dumpversion
	OUTPUT_VARIABLE CXX_COMPILER_VERSION
	)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

INCLUDE(CheckCXXSourceCompiles)

IF(CMAKE_COMPILER_IS_GNUCXX AND ${CMAKE_CXX_COMPILER} VERSION_LESS 4.2)
	MESSAGE("GCC older than 4.2 - not using the compiler's unordered map due to occasional observed bugs with -O2")
ELSE(CMAKE_COMPILER_IS_GNUCXX && ${CMAKE_CXX_COMPILER} VERSION_LESS 4.2)
	SET(unordered_map_list "unordered_map" "tr1/unordered_map")
	SET(namespace_list "std" "std::tr1")
	FOREACH(header ${unordered_map_list})
		FOREACH(namespace ${namespace_list})
			STRING(REPLACE "/" "_" header_str ${header})
			STRING(REPLACE ":" "_" namespace_str ${namespace})
			IF(NOT HASH_MAP_CLASS)
				SET(unordered_map_src "
#include <${header}>
int main(int argc, char *argv[]) {
  const ${namespace}::unordered_map<int, int> t;
  return t.find(5) == t.end();
}
")
            CHECK_CXX_SOURCE_COMPILES("${unordered_map_src}" ${header_str}_${namespace_str}_SUCCESS)
			ENDIF(NOT HASH_MAP_CLASS)
			IF(${header_str}_${namespace_str}_SUCCESS)	
				SET(HASH_MAP_H "<${header}>")
				SET(HASH_NAMESPACE "${namespace}")
				SET(HASH_MAP_CLASS "unordered_map")
			ENDIF(${header_str}_${namespace_str}_SUCCESS)	
		ENDFOREACH(namespace ${namespace_list})
	ENDFOREACH(header ${unordered_map_list})
ENDIF(CMAKE_COMPILER_IS_GNUCXX AND ${CMAKE_CXX_COMPILER} VERSION_LESS 4.2)

IF(NOT HASH_MAP_CLASS)
	SET(hash_map_list "ext/hash_map" "hash_map")
	SET(namespace_list "__gnu_cxx" "std" "stdext")
	FOREACH(header ${hash_map_list})
		FOREACH(namespace ${namespace_list})
			STRING(REPLACE "/" "_" header_str ${header})
			STRING(REPLACE ":" "_" namespace_str ${namespace})
			IF(NOT HASH_MAP_CLASS)
				SET(hash_map_src "
#include <${header}>
int main(int argc, char *argv[]) {
  ${namespace}::hash_map<int, int> t;
}
")
            CHECK_CXX_SOURCE_COMPILES("${hash_map_src}" ${header_str}_${namespace_str}_SUCCESS)
			ENDIF(NOT HASH_MAP_CLASS)
			IF(${header_str}_${namespace_str}_SUCCESS)	
				SET(HASH_MAP_H "<${header}>")
				SET(HASH_NAMESPACE "${namespace}")
				SET(HASH_MAP_CLASS "hash_map")
			ENDIF(${header_str}_${namespace_str}_SUCCESS)	
		ENDFOREACH(namespace ${namespace_list})
	ENDFOREACH(header ${hash_map_list})
ENDIF(NOT HASH_MAP_CLASS)

IF(HASH_MAP_CLASS)
	STRING(REGEX REPLACE "map" "set" HASH_SET_CLASS ${HASH_MAP_CLASS})
ENDIF(HASH_MAP_CLASS)
IF(HASH_MAP_H)
	STRING(REGEX REPLACE "map" "set" HASH_SET_H ${HASH_MAP_H})
ENDIF(HASH_MAP_H)

IF(HASH_MAP_CLASS AND HASH_MAP_H)
	SET(HAVE_HASH_MAP 1)
	SET(HAVE_HASH_SET 1)
ENDIF(HASH_MAP_CLASS AND HASH_MAP_H)
