//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2012 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENSE.GPL
//=============================================================================

#include <deque>
#include <QPushButton>
#include "musescore-gui-qt.h"
#include "inspector.h"
#include "inspectorTextLine.h"
#include "inspectorHairpin.h"
#include "libmscore/hairpin.h"
#include "libmscore/changeMap.h"

namespace Ms {

//---------------------------------------------------------
//   InspectorHairpin
//---------------------------------------------------------

InspectorHairpin::InspectorHairpin(QWidget* parent)
//   : InspectorElementBase(parent)
   : InspectorTextLineBase(parent)
      {
      h.setupUi(addWidget());

      h.hairpinType->clear();
      h.hairpinType->addItem(tr("Crescendo Hairpin"),   int(HairpinType::CRESC_HAIRPIN));
      h.hairpinType->addItem(tr("Decrescendo Hairpin"), int(HairpinType::DECRESC_HAIRPIN));
      h.hairpinType->addItem(tr("Crescendo Line"),      int(HairpinType::CRESC_LINE));
      h.hairpinType->addItem(tr("Decrescendo Line"),    int(HairpinType::DECRESC_LINE));

      h.veloChangeMethod->clear();
      h.veloChangeMethod->addItem(tr("Default (linear)"),   int(ChangeMethod::NORMAL));
      h.veloChangeMethod->addItem(tr("Ease-in and out"),    int(ChangeMethod::EASE_IN_OUT));
      h.veloChangeMethod->addItem(tr("Ease-in"),            int(ChangeMethod::EASE_IN));
      h.veloChangeMethod->addItem(tr("Ease-out"),           int(ChangeMethod::EASE_OUT));
      h.veloChangeMethod->addItem(tr("Exponential"),        int(ChangeMethod::EXPONENTIAL));

      const std::vector<InspectorItem> il = {
            { Pid::HAIRPIN_CIRCLEDTIP,   0, h.hairpinCircledTip,   h.resetHairpinCircledTip },
            { Pid::HAIRPIN_TYPE,         0, h.hairpinType,         0                        },
            { Pid::PLACEMENT,            0, h.placement,           h.resetPlacement         },
            { Pid::DYNAMIC_RANGE,        0, h.dynRange,            h.resetDynRange          },
            { Pid::VELO_CHANGE,          0, h.veloChange,          h.resetVeloChange        },
            { Pid::HAIRPIN_HEIGHT,       0, h.hairpinHeight,       h.resetHairpinHeight     },
            { Pid::HAIRPIN_CONT_HEIGHT,  0, h.hairpinContHeight,   h.resetHairpinContHeight },
            { Pid::SINGLE_NOTE_DYNAMICS, 0, h.singleNoteDynamics,  h.resetSingleNoteDynamics},
            { Pid::VELO_CHANGE_METHOD,   0, h.veloChangeMethod,    h.resetVeloChangeMethod  },
            };
      const std::vector<InspectorPanel> ppList = {
            { h.title, h.panel }
            };
      mapSignals(il, ppList);
      }

//---------------------------------------------------------
//   updateLineType
//---------------------------------------------------------

void InspectorHairpin::updateLineType()
      {
      HairpinSegment* hs = toHairpinSegment(inspector->element());
      Hairpin* hp = hs->hairpin();
      bool userDash = hp->lineStyle() == Qt::CustomDashLine;

      l.dashLineLength->setVisible(userDash);
      l.dashGapLength->setVisible(userDash);
      l.resetDashLineLength->setVisible(userDash);
      l.resetDashGapLength->setVisible(userDash);
      l.dashLineLengthLabel->setVisible(userDash);
      l.dashGapLengthLabel->setVisible(userDash);
      }

//---------------------------------------------------------
//   valueChanged
//---------------------------------------------------------

void InspectorHairpin::valueChanged(int idx)
      {
      InspectorTextLineBase::valueChanged(idx);
      if (iList[idx].t == Pid::LINE_STYLE)
            updateLineType();
      }

//---------------------------------------------------------
//   setElement
//---------------------------------------------------------

void InspectorHairpin::setElement()
      {
      InspectorTextLineBase::setElement();
      updateLineType();
      if (!h.singleNoteDynamics->isChecked()) {
            h.labelVeloChangeMethod->setEnabled(false);
            h.veloChangeMethod->setEnabled(false);
            h.resetVeloChangeMethod->setEnabled(false);
            }
      }

//---------------------------------------------------------
//   postInit
//---------------------------------------------------------

void InspectorHairpin::postInit()
      {
      bool useTextLine = h.hairpinType->currentIndex() == int(HairpinType::CRESC_LINE)
         || h.hairpinType->currentIndex() == int(HairpinType::DECRESC_LINE);
      l.lineVisible->setEnabled(useTextLine);
      h.gridWidget->setHidden(useTextLine);
      }

}

