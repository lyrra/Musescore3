
set(MODULE muxtools)

set (FLUID_DIR ${CMAKE_CURRENT_LIST_DIR}/fluid)
set (FLUID_GUI_SRC
    ${FLUID_DIR}/fluidgui.cpp
    ${FLUID_DIR}/fluidgui.h
    )

set (ZERBERUS_DIR ${CMAKE_CURRENT_LIST_DIR}/zerberus)
set (ZERBERUS_GUI_SRC
    ${ZERBERUS_DIR}/zerberusgui.cpp
    ${ZERBERUS_DIR}/zerberusgui.h
    )

if (APPLE)
      file(GLOB_RECURSE INCS "*.h")
else (APPLE)
      set(INCS "")
endif (APPLE)

include_directories (
      ${QT_INCLUDES}
      ${PROJECT_SOURCE_DIR}
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_CURRENT_LIST_DIR}
      ${PROJECT_SOURCE_DIR}/mux
      ${PROJECT_SOURCE_DIR}/muxlib
      ${PROJECT_SOURCE_DIR}/muxseq
      )

# QT5_WRAP_UI (ui_headers
#     ${MIDI_UI}
#     )

#if (USE_LAME)
      set (LAME_SRC exportmp3.h exportmp3.cpp)
#endif (USE_LAME)

add_library (${MODULE} SHARED
      muxseqtools.h muxseqtools.cpp
      eventutils.h eventutils.cpp
      exports/exportmidi.h exports/exportmidi.cpp
      synthesizergui.h synthesizergui.cpp
      midi/midifile.h midi/midifile.cpp
      ../muxlib/muxseq_net.h ../muxlib/muxseq_net.cpp
      #${FLUID_GUI_SRC}
      #${ZERBERUS_GUI_SRC}
      ${LAME_SRC}
      )

target_link_libraries (${MODULE}
      ${QT_LIBRARIES}
      musescore
      )

if (NOT MSVC)
   set_target_properties (
      ${MODULE}
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

