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

#include <stdio.h>
#include "all.h"
#include "mtest/testutils.h"

int g_argc;
char** g_argv;

int run_scheme_script(const char *filename, int rargc, char **rargv);

static QFile logFile;
static int processed = 0;
static int failed = 0;

Ms::MTest* g_mtest;
int g_test_check_pass_tot = 0;
int g_test_check_fail_tot = 0;
int g_test_check_pass = 0;
int g_test_check_fail = 0;

const char* tests[] = {
      //"libmscore/compat/tst_compat",          // expected to not work
      "libmscore/tst_element.scm",
      "libmscore/tst_note.scm",
      "libmscore/tst_hairpin.scm",
      "libmscore/tst_breath.scm",
      "libmscore/tst_copypaste.scm",
      "libmscore/tst_readwriteundoreset.scm",

      "libmscore/keysig/tst_keysig",
      "libmscore/barline/tst_barline",
      "libmscore/clef/tst_clef",
      "libmscore/timesig/tst_timesig",
      "libmscore/repeat/tst_repeat",
      "libmscore/rhythmicGrouping/tst_rhythmicGrouping",
      "libmscore/dynamic/tst_dynamic",
      "libmscore/durationtype/tst_durationtype",
      "libmscore/tuplet/tst_tuplet",
      "libmscore/chordsymbol/tst_chordsymbol",
      //"libmscore/text/tst_text",
      "libmscore/tempotext/tst_tempotext",
      "libmscore/measure/tst_measure",
      "libmscore/beam/tst_beam",
      "libmscore/layout/tst_benchmark",
      "libmscore/layout_elements/tst_layout_elements",
      "libmscore/instrumentchange/tst_instrumentchange",
      "libmscore/join/tst_join",
      "libmscore/transpose/tst_transpose",
      "libmscore/copypaste/tst_copypaste",
      //"libmscore/concertpitch/tst_concertpitchbenchmark",
      "libmscore/selectionfilter/tst_selectionfilter",
      "libmscore/tools/tst_tools",
      //"libmscore/plugins/tst_plugins",
      //"libmscore/album/tst_album",
      //"scripting/tst_scripting",
      //"guitarpro/tst_guitarpro",
      //"biab/tst_biab",
      "capella/io/tst_capella_io",
      //"importmidi/tst_importmidi",
      "libmscore/selectionrangedelete/tst_selectionrangedelete",
       "libmscore/parts/tst_parts",
      "testscript/tst_runscripts",
      //"mscore/palette/tst_palette", // disabled: No workspaces found

      "libmscore/spanners/tst_spanners",

      "libmscore/clef_courtesy/tst_clef_courtesy",
      //"libmscore/midimapping/tst_midimapping",
      "libmscore/earlymusic/tst_earlymusic",

      "libmscore/midi/tst_midi",
      "libmscore/splitstaff/tst_splitstaff",
      //"libmscore/split/tst_split",
      "libmscore/copypastesymbollist/tst_copypastesymbollist",

      // import/export
      //"testoves/structure/tst_ove_structure",
      //"testoves/ove3/tst_ove_ove3",
      //"testoves/bdat/tst_ove_bdat",
      //"musicxml/io/tst_mxml_io",
#ifdef OMR
      "omr/notes/tst_notes",
#endif
      };

//---------------------------------------------------------
//   process script or executable
//---------------------------------------------------------

static void run_process(const QString& cmd)
      {
      QStringList args;
      int rv = QProcess::execute(cmd, args);
      if (rv != 0) {
            // seems not to be reliable
            printf("========mtest process <%s> returns %d\n", qPrintable(cmd), rv);
            failed++;
            }
      processed++;
      }

static void run_script (const char* testname)
      {
      printf("\n======== running script %s ========\n", testname);
      g_test_check_pass = 0;
      g_test_check_fail = 0;
      run_scheme_script(testname, g_argc, g_argv);
      fprintf(stderr, "\n======== DONE running script %s ========\n", testname);
      if(g_test_check_fail > 0 || g_test_check_pass == 0) {
            printf("========mtest script <%s> checks: passed=%i failed=%i\n", testname, g_test_check_pass, g_test_check_fail);
            failed++;
            }
      processed++;
      g_test_check_pass_tot += g_test_check_pass;
      g_test_check_fail_tot += g_test_check_fail;
      }

static void run_script_or_process (const char *testname)
      {
      if (strstr(testname, ".scm")) {
            printf("run scheme-script %s\n", testname);
            run_script(testname);
            }
      else {
            printf("run test-program %s\n", testname);
            run_process(testname);
            }
      }
//---------------------------------------------------------
//   scanDir
//---------------------------------------------------------
#if 0
static void scanDir(QDir d)
      {
      QFileInfoList l = d.entryInfoList(QDir::NoDotAndDotDot | QDir::Files | QDir::Dirs);
      foreach(const QFileInfo& fi, l) {
            if (fi.isDir()) {
                  scanDir(QDir(fi.filePath()));
                  }
            else if (fi.isExecutable()) {
                  QString s(fi.filePath());
                  if (fi.completeBaseName().startsWith("tst_"))
                        process(s);
                  }
            }
      }
#endif

//---------------------------------------------------------
//   main
//---------------------------------------------------------

QCoreApplication* createApplication(int &argc, char *argv[])
{
      for (int i = 1; i < argc; ++i) {
            if (!qstrcmp(argv[i], "-no-gui"))
                  return new QCoreApplication(argc, argv);
            }
      return new QApplication(argc, argv);
      }

int main(int argc, char* argv[])
      {
      QScopedPointer<QCoreApplication> app(createApplication(argc, argv));
      g_mtest = new Ms::MTest();
      g_mtest->initMTest();
      // these are passed to scheme during script load/run
      g_argc = argc;
      g_argv = argv;
#if 0
      logFile.setFileName("mtest.log");
      if (!logFile.open(QIODevice::WriteOnly)) {
            fprintf(stderr, "mtest: cannot open log file <mtest.log>\n");
            exit(-1);
            }
#endif
      QDir wd(QDir::current());
#ifdef Q_OS_MAC
      wd.cdUp();
#endif

#if 0
      scanDir(wd);
#else
      printf("run scripts or processes\n");
      for (const char* s : tests)
            run_script_or_process(s);
#endif

      printf("\n");
      printf("================\n");
      printf("  Checks pass/fail %d/%d\n", g_test_check_pass_tot, g_test_check_fail_tot);
      printf("  processed %d  -- failed %d\n", processed, failed);
      printf("================\n");
      return 0;
      }

