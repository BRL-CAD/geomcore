# Find the libraries associated with Subversion
#
# Unlike the standard CMake module for finding Subversion, this module looks
# for the libraries and headers needed to build against the subversion libraries
# themselves.
#
# This module defines
#
#  SVN_INCLUDE_DIR, where to find svn_client.h
#  SVN_CLIENT_LIBRARY, the svn client library
#  SVN_REPOS_LIBRARY, the svn repository library

FIND_PATH(SVN_INCLUDE_DIR NAMES svn_client.h PATH_SUFFIXES subversion-1)

FIND_LIBRARY(SVN_CLIENT_LIBRARY NAMES svn_client-1 svn_client-1.0)
FIND_LIBRARY(SVN_REPOS_LIBRARY NAMES svn_repos-1 svn_repos-1.0)

SET(PACKAGE_HANDLE_VARS SVN_INCLUDE_DIR SVN_CLIENT_LIBRARY SVN_REPOS_LIBRARY)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(SVN DEFAULT_MSG ${PACKAGE_HANDLE_VARS})

MARK_AS_ADVANCED(SVN_INCLUDE_DIR SVN_CLIENT_LIBRARY SVN_REPOS_LIBRARY)

