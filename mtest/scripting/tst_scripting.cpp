//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2012 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#include <QtTest/QtTest>
#include "mtest/testutils.h"
#include "libmscore/score.h"
#include "libmscore/mscore.h"
#include "libmscore/musescoreCore.h"
#include "libmscore/undo.h"
#include "mscore/plugin/qmlplugin.h"
#include "mscore/plugin/qmlpluginengine.h"

#define DIR QString("scripting/")

using namespace Ms;

//---------------------------------------------------------
//   TestScripting
//---------------------------------------------------------

class TestScripting : public QObject, public MTest
      {
      Q_OBJECT

      QQmlEngine* engine;

      QmlPlugin* loadPlugin(QString path);
      void runPlugin(QmlPlugin* p, Score* cs);

   private slots:
      void initTestCase();
      void plugins01();
      void plugins02();
      void processFileWithPlugin_data();
      void processFileWithPlugin();
      void testTextStyle();
      };

//---------------------------------------------------------
///   runPlugin
//---------------------------------------------------------

void TestScripting::runPlugin(QmlPlugin* p, Score* cs)
      {
      // don't call startCmd for non modal dialog
      if (cs && p->pluginType() != "dock")
            cs->startCmd();
      p->runPlugin();
      if (cs && p->pluginType() != "dock")
            cs->endCmd();
      }

//---------------------------------------------------------
///   loadPlugin
///   Loads the qml plugin located at path
///   Returns pointer to the plugin or nullptr upon failure
///   Note: ensure to cleanup the returned pointer
//---------------------------------------------------------

QmlPlugin* TestScripting::loadPlugin(QString path)
      {
      QQmlComponent component(engine);
      component.loadUrl(QUrl::fromLocalFile(path));
      QObject* obj = component.create();
      if (obj == 0) {
            foreach(QQmlError e, component.errors())
                  qDebug("   line %d: %s", e.line(), qPrintable(e.description()));
            return nullptr;
            }

      return qobject_cast<QmlPlugin*>(obj);
      }

//---------------------------------------------------------
//   initTestCase
//---------------------------------------------------------

void TestScripting::initTestCase()
      {
      initMTest();
//       qmlRegisterType<MScore>    ("MuseScore", 1, 0, "MScore");
      engine = new QmlPluginEngine(this);
      }

//---------------------------------------------------------
///   plugins01
///   Create a QML item and retrieve its coordinates
//---------------------------------------------------------

void TestScripting::plugins01()
      {
      QString path = root + "/" + DIR + "plugins01.qml";
      QQmlComponent component(engine, QUrl::fromLocalFile(path));
      QObject* object = component.create();
      if (object == 0) {
            qDebug("creating component <%s> failed", qPrintable(path));
            foreach(QQmlError e, component.errors())
                  qDebug("   line %d: %s", e.line(), qPrintable(e.description()));
            }
      else {
            qreal x = object->property("x").toDouble();
            qreal y = object->property("y").toDouble();
            QCOMPARE(x, 50.0);
            QCOMPARE(y, 60.0);
            }
      delete object;
      }

//---------------------------------------------------------
///   plugin02
///   Create a MuseScore plugin and get width and height of the dialog
//---------------------------------------------------------

void TestScripting::plugins02()
      {
      QString path = root + "/" + DIR + "plugins02.qml";
      QQmlComponent component(engine,
         QUrl::fromLocalFile(path));
      QObject* object = component.create();
      if (object == 0) {
            qDebug("creating component <%s> failed", qPrintable(path));
            foreach(QQmlError e, component.errors())
                  qDebug("   line %d: %s", e.line(), qPrintable(e.description()));
            }
      else {
            qreal width  = object->property("width").toDouble();
            qreal height = object->property("height").toDouble();
            QCOMPARE(width, 150.0);
            QCOMPARE(height, 75.0);
            }
      delete object;
      }

//---------------------------------------------------------
//   processFileWithPlugin
//   read a score, apply script and compare script output with
//    reference
//---------------------------------------------------------

void TestScripting::processFileWithPlugin_data()
      {
      QTest::addColumn<QString>("file");
      QTest::addColumn<QString>("script");

      QTest::newRow("p1") << "s1" << "p1"; // scan note rest
      QTest::newRow("p2") << "s2" << "p2"; // scan segment attributes
      }

void TestScripting::processFileWithPlugin()
      {
      QFETCH(QString, file);
      QFETCH(QString, script);

      MasterScore* score = readScore(DIR + file + ".mscx");
      MuseScoreCore::mscoreCore->setCurrentScore(score);

      QVERIFY(score);
      score->doLayout();

      QString scriptPath = root + "/" + DIR + script + ".qml";

      QFileInfo fi(scriptPath);
      QVERIFY(fi.exists());

      QQmlComponent component(engine);
      component.loadUrl(QUrl::fromLocalFile(scriptPath));
      if (component.isError()) {
            qDebug("qml load error");
            for (QQmlError e : component.errors()) {
                  qDebug("qml error: %s", qPrintable(e.toString()));
                  }
            }

      QObject* obj = component.create();
      QVERIFY(obj);

      QmlPlugin* item = qobject_cast<QmlPlugin*>(obj);
      item->runPlugin();

      QVERIFY(compareFiles(script + ".log", DIR + script + ".log.ref"));
      delete score;
      }

//---------------------------------------------------------
///   testTextStyle
///   Reading and writing of a text style through the plugin framework
//---------------------------------------------------------

void TestScripting::testTextStyle()
      {
      QmlPlugin* item = loadPlugin(root + "/" + DIR + "testTextStyle.qml");
      QVERIFY(item != nullptr);

      Score* score = readScore(DIR + "testTextStyle.mscx");
      MuseScoreCore::mscoreCore->setCurrentScore(score);
      runPlugin(item, score);
      QVERIFY(saveCompareScore(score, "testTextStyle-test.mscx", DIR + "testTextStyle-ref.mscx"));
      score->undoRedo(/* undo */ true, /* EditData */ nullptr);
      QVERIFY(saveCompareScore(score, "testTextStyle-test2.mscx", DIR + "testTextStyle.mscx"));

      delete item;
      }

QTEST_MAIN(TestScripting)
#include "tst_scripting.moc"

