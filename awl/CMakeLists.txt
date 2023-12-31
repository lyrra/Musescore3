#=============================================================================
#  MusE
#  Linux Music Editor
#
#  Copyright (C) 2002-2016 by Werner Schweer and others
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 2.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#=============================================================================

add_library (
      awl STATIC
      aslider.cpp
      knob.cpp
      panknob.cpp
      volknob.cpp
      midipanknob.cpp
      colorlabel.cpp
      slider.cpp
      styledslider.cpp
      volslider.cpp
      mslider.cpp
      pitchedit.cpp
      pitchlabel.cpp
      poslabel.cpp
      utils.cpp
      denomspinbox.cpp
      aslider.h colorlabel.h denomspinbox.h fastlog.h
      knob.h midipanknob.h mslider.h panknob.h pitchedit.h pitchlabel.h
      poslabel.h slider.h styledslider.h
      utils.h volknob.h volslider.h
      )
if (NOT MSVC)
   set_target_properties (
      awl
      PROPERTIES
         COMPILE_FLAGS "-g -Wall -Wextra"
      )
endif (NOT MSVC)   

include_directories(${QT_INCLUDES})

add_library (
      awlplugin SHARED
      awlplugin.cpp
      aslider.cpp
      knob.cpp
      panknob.cpp
      volknob.cpp
      midipanknob.cpp
      colorlabel.cpp
      slider.cpp
      styledslider.cpp
      volslider.cpp
      mslider.cpp
      denomspinbox.cpp
      pitchedit.cpp
      pitchlabel.cpp
      utils.cpp
      )

#
# We cannot use our precompiled headers because of -fPIC
#
if (NOT MSVC)
   set_target_properties(awlplugin
      PROPERTIES
      COMPILE_FLAGS
         "-fPIC -D_GNU_SOURCE -D_REENTRANT -DHAVE_CONFIG_H -DQT_PLUGIN -DQT_SHARED -DQT_NO_DEBUG -include"
      )
else (NOT MSVC)
   # x86 architecture does not support position-independent code, the -fPIC option is not relevant for this processor.
   set_target_properties (
      awlplugin
      PROPERTIES
         COMPILE_FLAGS "-D_GNU_SOURCE -D_REENTRANT -DHAVE_CONFIG_H -DQT_PLUGIN -DQT_SHARED -DQT_NO_DEBUG"
      )
endif (NOT MSVC)   

# Don't install awlplugin on default
#install( TARGETS awlplugin DESTINATION ${designerPlugins}/plugins/designer/ )

#add_executable(ltest ltest.cpp)
#target_link_libraries (ltest awlplugin msynth al ${QT_LIBRARIES})

set_target_properties(awlplugin PROPERTIES EXCLUDE_FROM_ALL 1)

