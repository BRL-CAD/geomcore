# Minimum required version of CMake
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
IF(COMMAND CMAKE_POLICY)
	CMAKE_POLICY(SET CMP0003 NEW)
ENDIF(COMMAND CMAKE_POLICY)

# set CMake project name
PROJECT(GLOG)

# Set version
SET(GLOG_VERSION_MAJOR "0")
SET(GLOG_VERSION_MINOR "3")
SET(GLOG_VERSION_PATCH "1")
SET(GLOG_VERSION "${GLOG_VERSION_MAJOR}.${GLOG_VERSION_MINOR}.${GLOG_VERSION_PATCH}")

# Assume STDC unless/until we hit a situation were we don't have 'em
SET(STDC_HEADERS 1)

INCLUDE(CheckIncludeFile)
CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILE(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(syscall.h HAVE_SYSCALL_H)
CHECK_INCLUDE_FILE(sys/syscall.h HAVE_SYS_SYSCALL_H)
# For backtrace with glibc.
CHECK_INCLUDE_FILE(execinfo.h HAVE_EXECINFO_H)
# For backtrace with libunwind.
CHECK_INCLUDE_FILE(libunwind.h HAVE_LIBUNWIND_H)
CHECK_INCLUDE_FILE(ucontext.h HAVE_UCONTEXT_H)
CHECK_INCLUDE_FILE(sys/utsname.h HAVE_SYS_UTSNAME_H)
CHECK_INCLUDE_FILE(pwd.h HAVE_PWD_H)
CHECK_INCLUDE_FILE(syslog.h HAVE_SYSLOG_H)
CHECK_INCLUDE_FILE(sys/time.h HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILE(glob.h HAVE_GLOB_H)

SET(SIZEOF_VOID_P ${CMAKE_SIZEOF_VOID_P})

# These are the types I need.  We look for them in either stdint.h,
# sys/types.h, or inttypes.h, all of which are part of the default-includes.
INCLUDE(CheckTypeSize)
CHECK_TYPE_SIZE(uint16_t HAVE_UINT16_T)
CHECK_TYPE_SIZE(u_int16_t HAVE_U_INT16_T)
CHECK_TYPE_SIZE(__uint16 HAVE__UINT16)

INCLUDE(CheckFunctionExists)
CHECK_FUNCTION_EXISTS(sigaltstack HAVE_SIGALTSTACK)
CHECK_FUNCTION_EXISTS(dladdr HAVE_DLADDR)
CHECK_FUNCTION_EXISTS(fcntl HAVE_FCNTL)

INCLUDE(CheckCXXSourceCompiles)
SET(attribute_src "
#include <stdlib.h>
static void foo(void) __attribute__ ((unused))\;
void foo(void) { exit(1)\; }
int main () { return 0\; }
")
CHECK_CXX_SOURCE_COMPILES(${attribute_src} HAVE___ATTRIBUTE__)
IF(HAVE___ATTRIBUTE__)
	SET(ac_cv___attribute___noreturn "__attribute__ ((noreturn))")
	SET(ac_cv___attribute___printf_4_5 "__attribute__((__format__ (__printf__,	4, 5)))")
ENDIF(HAVE___ATTRIBUTE__)

SET(builtin_expect_src "
int foo(void) { if (__builtin_expect(0, 0)) return 1\; return 0\; }
int main () { return 0\; }
")
CHECK_CXX_SOURCE_COMPILES(${builtin_expect_src} HAVE___BUILTIN_EXPECT)
IF(HAVE___BUILTIN_EXPECT)
	SET(ac_cv_have___builtin_expect 1)
ENDIF(HAVE___BUILTIN_EXPECT)

#AX_C___SYNC_VAL_COMPARE_AND_SWAP

IF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")
	IF(NOT DEFINED ENABLE_FRAME_POINTERS)
		OPTION(ENABLE_FRAME_POINTERS ON "Enable Frame pointers on x86_64")
	ENDIF(NOT DEFINED ENABLE_FRAME_POINTERS)
	#[On x86_64 systems, compile with -fno-omit-frame-pointer (see INSTALL)]),,
ENDIF(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "x86_64")

FIND_PACKAGE(Threads)
IF(CMAKE_USE_PTHREADS_INIT)
	SET(HAVE_PTHREAD 1)
	SET(HAVE_LIBPTHREAD 1)
	SET(rwlock_srcs "
#define _XOPEN_SOURCE 500
#include <pthread.h>
int main() {
   pthread_rwlock_t l\; 
   pthread_rwlock_init(&l, NULL)\;
   pthread_rwlock_rdlock(&l)\;
	return 0\;
	}
")
CHECK_CXX_SOURCE_COMPILES(${rwlock_srcs} HAVE_RWLOCK)
ENDIF(CMAKE_USE_PTHREADS_INIT)

IF(GFLAGS_LIBRARY AND GFLAGS_INCLUDE_DIR)
	SET(HAVE_GFLAGS 1)
	SET(HAVE_LIB_GFLAGS 1)
ENDIF(GFLAGS_LIBRARY AND GFLAGS_INCLUDE_DIR)

IF(GMOCK_LIBRARY AND GMOCK_INCLUDE_DIR)
	SET(HAVE_LIB_GMOCK 1)
ENDIF(GMOCK_LIBRARY AND GMOCK_INCLUDE_DIR)

IF(GTEST_LIBRARY AND GTEST_INCLUDE_DIR)
	SET(HAVE_LIB_GTEST 1)
ENDIF(GTEST_LIBRARY AND GTEST_INCLUDE_DIR)

#libunwind
INCLUDE(CheckLibraryExists)
CHECK_LIBRARY_EXISTS(unwind backtrace "" HAVE_UNWIND_LIB)
CHECK_INCLUDE_FILE(unwind.h HAVE_UNWIND_H)
IF(HAVE_UNWIND_H AND HAVE_UNWIND_LIB)
	SET(HAVE_LIB_UNWIND 1)
ENDIF(HAVE_UNWIND_H AND HAVE_UNWIND_LIB)

# Find out what namespace 'normal' STL code lives in, and also what namespace
# the user wants our classes to be defined in
SET(stl_none_src"
#include <vector>
int main () { vector<int> t\; return 0\; }
")
CHECK_CXX_SOURCE_COMPILES(${stl_none_src} STL_NAMESPACE_NONE)
IF(NOT STL_NAMESPACE_NONE)
	SET(stl_std_src"
#include <vector>
int main () { std::vector<int> t\; return 0\; }
")
	CHECK_CXX_SOURCE_COMPILES(${stl_std_src} STL_NAMESPACE_STD)
	IF(STL_NAMESPACE_STD)
		SET(STL_NAMESPACE "std")
	ENDIF(STL_NAMESPACE_STD)
ENDIF(NOT STL_NAMESPACE_NONE)

SET(GOOGLE_NAMESPACE "google")
SET(_START_GOOGLE_NAMESPACE_ "namespace ${GOOGLE_NAMESPACE} {")
SET(_END_GOOGLE_NAMESPACE_ "}")
SET(google_namespace "${GOOGLE_NAMESPACE}")
SET(google_start_namespace "${_START_GOOGLE_NAMESPACE_}")
SET(google_end_namespace "${_END_GOOGLE_NAMESPACE_}")

#AC_CXX_USING_OPERATOR

#AC_PC_FROM_UCONTEXT(AC_MSG_WARN(Could not find the PC.  Will not output failed addresses...))

#AC_DEFINE_UNQUOTED(TEST_SRC_DIR, "$srcdir", [location of source code])

# These are what's needed by logging.h.in and raw_logging.h.in
#AC_SUBST(ac_google_start_namespace)
#AC_SUBST(ac_google_end_namespace)
#AC_SUBST(ac_google_namespace)
#AC_SUBST(ac_cv_cxx_using_operator)
#AC_SUBST(ac_cv___attribute___noreturn)
#AC_SUBST(ac_cv___attribute___printf_4_5)
#AC_SUBST(ac_cv_have___builtin_expect)
#AC_SUBST(ac_cv_have_libgflags)
#AC_SUBST(GFLAGS_CFLAGS)
#AC_SUBST(GTEST_CFLAGS)
#AC_SUBST(GMOCK_CFLAGS)
#AC_SUBST(GFLAGS_LIBS)
#AC_SUBST(GTEST_LIBS)
#AC_SUBST(GMOCK_LIBS)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/config_cmake.h.in ${CMAKE_CURRENT_BINARY_DIR}/src/config.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/glog/logging.h.in ${CMAKE_CURRENT_BINARY_DIR}/src/glog/logging.h @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/glog/raw_logging.h.in	${CMAKE_CURRENT_BINARY_DIR}/src/glog/raw_logging.h @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/glog/vlog_is_on.h.in ${CMAKE_CURRENT_BINARY_DIR}/src/glog/vlog_is_on.h @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/glog/stl_logging.h.in ${CMAKE_CURRENT_BINARY_DIR}/src/glog/stl_logging.h @ONLY)

ADD_SUBDIRECTORY(src)
#AC_OUTPUT(libglog.pc)
