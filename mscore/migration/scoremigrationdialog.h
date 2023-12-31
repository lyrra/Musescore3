#ifndef SCOREMIGRATIONDIALOG_H
#define SCOREMIGRATIONDIALOG_H

#include <QQmlEngine>
#include <QFocusEvent>
#include <QQuickView>
#include "musescore-qt.h"
#include "scoremigrationdialogmodel.h"

class ScoreMigrationDialog : public QQuickView
      {
      Q_OBJECT

   public:
      explicit ScoreMigrationDialog(QQmlEngine* engine, Ms::Score *score);

   private:
      void focusInEvent(QFocusEvent* event) override;

      ScoreMigrationDialogModel* m_dialogModel = nullptr;
      };

#endif // SCOREMIGRATIONDIALOG_H
