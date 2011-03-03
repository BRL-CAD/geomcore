include(ExternalProject)

MACRO(THIRD_PARTY_CONFIGURE_EXTERNAL_PROJECT upper projname projpath srcpath extraopts)
		ExternalProject_Add(
			${projname}
			DOWNLOAD_COMMAND ""
			PREFIX ${CMAKE_CURRENT_BINARY_DIR}
			SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${projpath}/${srcpath}
			CONFIGURE_COMMAND mkdir -p
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && cd
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ &&
			<SOURCE_DIR>/configure --prefix=${CMAKE_INSTALL_PREFIX} --exec-prefix=${CMAKE_INSTALL_PREFIX} --mandir=${${CMAKE_PROJECT_NAME}_INSTALL_MAN_DIR} ${extraopts}
			BUILD_COMMAND cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE)
			INSTALL_COMMAND  cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE) install
			)
		SET(CMAKE_EXTERNAL_TARGET_LIST "${CMAKE_EXTERNAL_TARGET_LIST};${projname}" CACHE STRING "external target list" FORCE)
ENDMACRO(THIRD_PARTY_CONFIGURE_EXTERNAL_PROJECT)

MACRO(THIRD_PARTY_AUTOCONF_EXTERNAL_PROJECT upper projname projpath srcpath extraopts)
		ExternalProject_Add(
			${projname}
			DOWNLOAD_COMMAND ""
			PREFIX ${CMAKE_CURRENT_BINARY_DIR}
			SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${projpath}/${srcpath}
			CONFIGURE_COMMAND autoconf -I <SOURCE_DIR> -o
			<SOURCE_DIR>/configure <SOURCE_DIR>/configure.in && mkdir -p
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && cd
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ &&
			<SOURCE_DIR>/configure --prefix=${CMAKE_INSTALL_PREFIX} --exec-prefix=${CMAKE_INSTALL_PREFIX} --mandir=${${CMAKE_PROJECT_NAME}_INSTALL_MAN_DIR} ${extraopts}
			BUILD_COMMAND cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE)
			INSTALL_COMMAND  cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE) install
			)
		SET(CMAKE_EXTERNAL_TARGET_LIST "${CMAKE_EXTERNAL_TARGET_LIST};${projname}" CACHE STRING "external target list" FORCE)
ENDMACRO(THIRD_PARTY_AUTOCONF_EXTERNAL_PROJECT)


MACRO(THIRD_PARTY_AUTORECONF_EXTERNAL_PROJECT upper projname projpath srcpath extraopts)
		ExternalProject_Add(
			${projname}
			DOWNLOAD_COMMAND ""
			PREFIX ${CMAKE_CURRENT_BINARY_DIR}
			SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${projpath}/${srcpath}
			CONFIGURE_COMMAND cd <SOURCE_DIR> && autoreconf -i -f && mkdir -p
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && cd
			${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ &&
			<SOURCE_DIR>/configure --prefix=${CMAKE_INSTALL_PREFIX} --exec-prefix=${CMAKE_INSTALL_PREFIX} --mandir=${${CMAKE_PROJECT_NAME}_INSTALL_MAN_DIR} ${extraopts}
			BUILD_COMMAND cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE)
			INSTALL_COMMAND  cd ${CMAKE_CURRENT_BINARY_DIR}/${projpath}/ && $(MAKE) install
			)
		SET(CMAKE_EXTERNAL_TARGET_LIST "${CMAKE_EXTERNAL_TARGET_LIST};${projname}" CACHE STRING "external target list" FORCE)
ENDMACRO(THIRD_PARTY_AUTORECONF_EXTERNAL_PROJECT)

