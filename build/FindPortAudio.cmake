pkg_check_modules(PORTAUDIO "portaudio-2.0")

find_path(PORTAUDIO_INCLUDE_DIR portaudio.h)
find_library(PORTAUDIO_LIBRARY NAMES portaudio)

#add_library(portaudio SHARED IMPORTED)
#set_target_properties(portaudio PROPERTIES IMPORTED_IMPLIB ${PORTAUDIO_LIBRARIES})
#set ( PORTAUDIODLL portaudiodll )

message("Found PortAudio: ${PORTAUDIO_FOUND}")
if (PORTAUDIO_FOUND)
    message(STATUS "Found PortAudio: PORTAUDIO_LIBRARY_DIRS=${PORTAUDIO_LIBRARY_DIRS}")
    message(STATUS "Found PortAudio: PORTAUDIO_LIBRARY=${PORTAUDIO_LIBRARY}")
    message(STATUS "Found PortAudio: PORTAUDIO_LIBRARIES=${PORTAUDIO_LIBRARIES}")
    message(STATUS "Found PortAudio: PORTAUDIO_LINK_LIBRARIES=${PORTAUDIO_LINK_LIBRARIES}")
else (PORTAUDIO_FOUND)
    message (FATAL_ERROR "Could not find PortAudio")
endif (PORTAUDIO_FOUND)
