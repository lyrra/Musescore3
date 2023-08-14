# GPL-2.0-or-later

#if (APPLE)
#      file(GLOB_RECURSE INCS "*.h")
#else (APPLE)
#      set(INCS "")
#endif (APPLE)

#include_directories (
#      ${PROJECT_SOURCE_DIR}
#      ${CMAKE_CURRENT_BINARY_DIR}
#      ${CMAKE_CURRENT_LIST_DIR}
#      )

add_library(mux STATIC
      mux.h mux.cpp
      muxqueue.h muxqueue.cpp
      muxtaskv.h muxtaskv.cpp
      )

#target_link_libraries (${MODULE}
#      ${QT_LIBRARIES}
#      )

#if (NOT MSVC)
#   set_target_properties (
#      ${MODULE}
#      PROPERTIES
#         COMPILE_FLAGS "-g -Wall -Wextra"
#      )
#endif (NOT MSVC)   
