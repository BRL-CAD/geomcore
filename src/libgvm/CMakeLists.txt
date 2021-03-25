find_package(SubversionLibrary)
find_package(APR)

if (SUBVERSION_INCLUDE_DIRS)
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${SUBVERSION_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/src/other/subversion/include/private
    ${BRLCAD_INCLUDE_DIRS}
    )

  set(gvm_SRCS
    files.c
    mem.c
    models.c
    objects.c
    repo.c
    )

    add_library(libgvm SHARED ${gvm_SRCS})
    target_link_libraries(libgvm ${SUBVERSION_LIBRARIES} ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
    set_target_properties(libgvm PROPERTIES PREFIX "")
    install(TARGETS libgvm DESTINATION lib)
  if(BUILD_STATIC_LIBS)
    add_library(libgvm-static STATIC ${gvm_SRCS})
    target_link_libraries(libgvm-static svn_repos ${BRLCAD_GED_LIBRARY} ${BRLCAD_WDB_LIBRARY} ${BRLCAD_RT_LIBRARY} ${BRLCAD_BU_LIBRARY})
    if(NOT WIN32)
      set_target_properties(libgvm-static PROPERTIES PREFIX "")
      set_target_properties(libgvm-static PROPERTIES OUTPUT_NAME "libgvm")
    else(NOT WIN32)
      set_target_properties(libgvm-static PROPERTIES PREFIX "lib")
    endif(NOT WIN32)
    install(TARGETS libgvm-static DESTINATION lib)
  endif(BUILD_STATIC_LIBS)
endif (SUBVERSION_INCLUDE_DIRS)

# Local Variables:
# tab-width: 8
# mode: cmake
# indent-tabs-mode: t
# End:
# ex: shiftwidth=2 tabstop=8
