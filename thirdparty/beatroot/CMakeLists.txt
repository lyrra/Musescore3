if (APPLE)
      file(GLOB_RECURSE INCS "*.h")
else (APPLE)
      set(INCS "")
endif (APPLE)

add_library (
      beatroot STATIC
      Induction.cpp
      BeatTracker.cpp
      AgentList.cpp
      Agent.cpp
  )

if (NOT MSVC)
   set_target_properties( beatroot
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

