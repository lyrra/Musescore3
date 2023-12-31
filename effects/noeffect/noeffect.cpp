//=============================================================================
//  MuseSynth
//  Music Software Synthesizer
//
//  Copyright (C) 2013 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#include "musescore-qt.h"

#include "effects/effect.h"
#include "noeffect.h"

namespace Ms {

//---------------------------------------------------------
//   gui
//---------------------------------------------------------

#if 0
EffectGui* NoEffect::gui()
      {
      if (!_gui) {
            _gui = new NoEffectGui(this);
            _gui->setGeometry(0, 0, 644, 79);
            }
      return _gui;
      }

//---------------------------------------------------------
//   NoEffectGui
//---------------------------------------------------------

NoEffectGui::NoEffectGui(Effect* e, QWidget* parent)
   : EffectGui(e, parent)
      {
      QLabel* l = new QLabel;
      l->setText(tr("No Plugin"));
      QLayout* la = new QVBoxLayout;
      la->addWidget(l);
      setLayout(la);
      }
#endif

//---------------------------------------------------------
//   parDescr
//---------------------------------------------------------

const std::vector<ParDescr>& NoEffect::parDescr() const
      {
      static const std::vector<ParDescr> noeffectPd;
      return noeffectPd;
      }

//---------------------------------------------------------
//   process
//---------------------------------------------------------

void NoEffect::process(int n, float* src, float* dst)
      {
      memcpy(dst, src, n * 2 * sizeof(float));
      }

}

