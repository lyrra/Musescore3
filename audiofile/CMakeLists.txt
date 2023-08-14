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

if (APPLE)
        file(GLOB_RECURSE INCS "*.h")
else (APPLE)
        set(INCS "")
endif (APPLE)



add_library (audiofile STATIC
      audiofile.cpp
      )

target_include_directories(audiofile PRIVATE ${PROJECT_SOURCE_DIR}/include ${QT_INCLUDES})

if (MSVC OR MINGW)
 target_link_libraries(audiofile sndfiledll)
else (MSVC OR MINGW)
 target_link_libraries(audiofile ${SNDFILE_LIB})
endif (MSVC OR MINGW)

if (NOT MSVC)
   set_target_properties (
      audiofile
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

