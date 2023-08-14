
set(MODULE muxlib)

include_directories (
      ${QT_INCLUDES}
      ${PROJECT_SOURCE_DIR}
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_CURRENT_LIST_DIR}
      ${PROJECT_SOURCE_DIR}/mux
      ${PROJECT_SOURCE_DIR}/mscore
      ${PROJECT_SOURCE_DIR}/muxseq
      ${PROJECT_SOURCE_DIR}/thirdparty  # because include "mscore/musescore.h"
      )

add_library (${MODULE} SHARED
      midi/midipatch.h
      event.h event.cpp
      muxlib.h muxlib.cpp
      )
add_dependencies(${MODULE} generated_sources)

target_link_libraries (${MODULE}
      mux
      Qt5::Core)

if (NOT MSVC)
   set_target_properties (
      ${MODULE}
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

add_library(muxseqclient STATIC
    muxseq_client.cpp muxseq_client.h
    )

set_target_properties (
    muxseqclient
    PROPERTIES
        COMPILE_FLAGS "-g -Wall -Wextra"
    )

add_dependencies(muxseqclient generated_sources)

