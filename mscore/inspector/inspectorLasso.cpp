//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2011 Werner Schweer and others
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENSE.GPL
//=============================================================================

#include <QPushButton>
#include "musescore-gui-qt.h"
#include "inspectorLasso.h"
#include "libmscore/lasso.h"

namespace Ms {

//---------------------------------------------------------
//   InspectorLasso
//---------------------------------------------------------

InspectorLasso::InspectorLasso(QWidget* parent)
   : InspectorBase(parent)
      {
      b.setupUi(addWidget());

      iList = {
            { Pid::LASSO_POS,    0, b.pos,   0 },
            { Pid::LASSO_SIZE,   0, b.size,  0 },
            };
      pList = { { b.title, b.panel } };

      b.pos->setSuffix(tr("mm"));
      b.size->setSuffix(tr("mm"));

      mapSignals();
      }

}
