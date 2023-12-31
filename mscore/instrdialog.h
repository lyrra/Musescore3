//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2002-2014 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#ifndef __INSTRDIALOG_H__
#define __INSTRDIALOG_H__

#include "ui_instrdialog.h"
#include "abstractdialog.h"

namespace Ms {

class Score;

//---------------------------------------------------------
//   InstrumentsDialog
//---------------------------------------------------------

class InstrumentsDialog : public AbstractDialog, public Ui::InstrumentsDialog {
      Q_OBJECT

      void readSettings();
      virtual void accept();

   private slots:
      void buttonBoxClicked(QAbstractButton*);
      void on_saveButton_clicked();
      void on_loadButton_clicked();
      
   protected:
      virtual void retranslate();

   public:
      InstrumentsDialog(QWidget* parent = 0);
      void init();
      void writeSettings();
      void genPartList(Score*);
      void setBracketsAndBarlines(Score*);
      QTreeWidget* partiturList();
      void setScoreOrder(ScoreOrder* order);
      ScoreOrder* getScoreOrder();
      void buildInstrumentsList();
      };

} // namespace Ms


#endif

