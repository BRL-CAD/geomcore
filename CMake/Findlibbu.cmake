# - Try to find libbu
# Once done, this will define
#
#  libbu_FOUND - system has libbu
#  libbu_INCLUDE_DIRS - the libbu include directories
#  libbu_LIBRARIES - link these to use libbu

include(LibFindMacros)

# Use pkg-config to get hints about paths
libfind_pkg_check_modules(libbu_PKGCONF libbu)

set(libbu_LIB_PREFIX_GUESSES $ENV{BRLCAD_ROOT}/lib)
set(libbu_INC_PREFIX_GUESSES $ENV{BRLCAD_ROOT}/include)

if (WIN32)
  # TODO: Guess default install locations on windows
elseif (UNIX)
  set(libbu_LIB_PREFIX_GUESSES ${libbu_LIB_PREFIX_GUESSES} /usr/brlcad/lib /usr/local/brlcad/lib ~/lib)
  set(libbu_INC_PREFIX_GUESSES ${libbu_INC_PREFIX_GUESSES} /usr/brlcad/include /usr/local/brlcad/include ~/include)
endif ()

# Include dir
find_path(libbu_INCLUDE_DIR
  NAMES brlcad/bu.h
  PATHS ${libbu_PKGCONF_INCLUDEDIR} ${libbu_INC_PREFIX_GUESSES}
)

# Finally the library itself
find_library(libbu_LIBRARY
  NAMES bu
  PATHS ${libbu_PKGCONF_LIBDIR} ${libbu_LIB_PREFIX_GUESSES}
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
set(libbu_PROCESS_INCLUDES libbu_INCLUDE_DIR)
set(libbu_PROCESS_LIBS libbu_LIBRARY)
libfind_process(libbu)
