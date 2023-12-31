//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2002-2017 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#ifndef __PAGESETTINGS_H__
#define __PAGESETTINGS_H__

#include "ui_pagesettings.h"
#include "abstractdialog.h"

namespace Ms {

class MasterScore;
class Score;
class Navigator;

//---------------------------------------------------------
//   PageSettings
//---------------------------------------------------------

class PageSettings : public AbstractDialog, private Ui::PageSettingsBase {
      Q_OBJECT

      Navigator* preview = nullptr;
      bool mmUnit = false;
      bool _changeFlag = false;
      Score* cs = nullptr;
      Score* clonedScore = nullptr;

      virtual void hideEvent(QHideEvent*);
      void updateValues();
      void updatePreview();
      void blockSignals(bool);
      void applyToScore(Score*);
      void setMarginsMax(double);
      void apply();

   private slots:
      void mmClicked();
      void inchClicked();
      void pageFormatSelected(int);

      void applyToAllParts();
      void buttonBoxClicked(QAbstractButton*);

      void twosidedToggled(bool);
      void otmChanged(double val);
      void obmChanged(double val);
      void olmChanged(double val);
      void ormChanged(double val);
      void etmChanged(double val);
      void ebmChanged(double val);
      void elmChanged(double val);
      void ermChanged(double val);
      void spatiumChanged(double val);
      void pageHeightChanged(double);
      void pageWidthChanged(double);
      void pageOffsetChanged(int val);
      void orientationClicked();
      void on_resetPageStyleButton_clicked();

   protected:
      virtual void retranslate() { retranslateUi(this); }

   public:
      PageSettings(QWidget* parent = 0);
      ~PageSettings();
      void setScore(Score*);
      };


} // namespace Ms
#endif

