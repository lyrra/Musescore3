//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2018 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENSE.GPL
//=============================================================================

#include <QPushButton>
#include "musescore-gui-qt.h"
#include "inspector.h"
#include "inspectorPalmMute.h"

namespace Ms {

//---------------------------------------------------------
//   InspectorPalmMute
//---------------------------------------------------------

InspectorPalmMute::InspectorPalmMute(QWidget* parent)
   : InspectorTextLineBase(parent)
      {
      pm.setupUi(addWidget());

      const std::vector<InspectorItem> il = {
            { Pid::PLACEMENT,  0, pm.placement,  pm.resetPlacement             },
            };
      const std::vector<InspectorPanel> ppList = {
            { pm.title, pm.panel },
            };

      populatePlacement(pm.placement);
      mapSignals(il, ppList);
      }
}

