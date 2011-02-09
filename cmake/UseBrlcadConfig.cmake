# mafm: copied from UsePkgConfig.cmake, which is originally from
# https://wiki-flowvr.imag.fr/browser/trunk/flowvr-render/cmake/UsePkgConfig.cmake?rev=1793&format=raw

# Get the executable brlcad-config path, if exists
find_program(BRLCADCONFIG_EXECUTABLE NAMES brlcad-config PATHS ${BRLCAD_ROOT}/bin)


# Macro to check whether the package exist, with at least the given
# version
macro(BRLCADCONFIG_ATLEAST _atleast_version _found)
  # reset the variables at the beginning
  remove(${_found})

  # if brlcad-config has been found
  if(BRLCADCONFIG_EXECUTABLE)
    exec_program(${BRLCADCONFIG_EXECUTABLE} ARGS --version
      RETURN_VALUE _exitCode OUTPUT_VARIABLE _output )
    if(${_output} STRLESS ${_atleast_version})
      set(${_found} 0)
    else(${_output} STRLESS ${_atleast_version})
      set(${_found} 1)
    endif(${_output} STRLESS ${_atleast_version})
  endif(BRLCADCONFIG_EXECUTABLE)
endmacro(BRLCADCONFIG_ATLEAST _atleast_version _return)


# Macro to simplify calls of brlcad-config: it accepts arguments freely,
# and returns the output and the exit code
macro(BRLCADCONFIG_WRAPPER _args _output _exitCode)
  # reset the variables at the beginning
  remove(${_output})

  # if brlcad-config has been found
  if(BRLCADCONFIG_EXECUTABLE)
    exec_program(${BRLCADCONFIG_EXECUTABLE} ARGS ${_args} RETURN_VALUE ${_exitCode} OUTPUT_VARIABLE ${_output} )
  endif(BRLCADCONFIG_EXECUTABLE)
endmacro(BRLCADCONFIG_WRAPPER _args _output _exitCode)


mark_as_advanced(BRLCADCONFIG_EXECUTABLE)
