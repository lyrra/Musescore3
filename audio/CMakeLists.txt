#=============================================================================
#  MuseScore
#  Music Composition & Notation
#
#  Copyright (C) 2013 Werner Schweer
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 2
#  as published by the Free Software Foundation and appearing in
#  the file LICENSE.GPL
#=============================================================================

set(MODULE audio)

if (APPLE)
      file(GLOB_RECURSE INCS "*.h")
else (APPLE)
      set(INCS "")
endif (APPLE)

include_directories (
      ${PROJECT_SOURCE_DIR}
      ${CMAKE_CURRENT_LIST_DIR}
      ${PROJECT_SOURCE_DIR}/global
      ${PROJECT_SOURCE_DIR}/thirdparty  # because include "mscore/musescore.h"
      )

QT5_WRAP_UI (ui_headers
    ${MIDI_UI}
    )

add_library (${MODULE} STATIC
      ${ui_headers}
      )

target_link_libraries (${MODULE}
      ${QT_LIBRARIES}
      audiofile
      )

if (NOT MSVC)
   set_target_properties (
      ${MODULE}
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

