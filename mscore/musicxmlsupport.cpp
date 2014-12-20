//=============================================================================
//  MusE Score
//  Linux Music Score Editor
//  $Id: musicxmlsupport.cpp 5595 2012-04-29 15:30:32Z lvinken $
//
//  Copyright (C) 2012 Werner Schweer and others
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//=============================================================================

/**
 MusicXML support.
 */

#include "globals.h"
#include "musicxmlsupport.h"
#include "libmscore/sym.h"

namespace Ms {

NoteList::NoteList()
      {
      for (int i = 0; i < MAX_STAVES; ++i)
            _staffNoteLists << StartStopList();
      }

void NoteList::addNote(const int startTick, const int endTick, const int staff)
      {
      if (staff >= 0 && staff < _staffNoteLists.size())
            _staffNoteLists[staff] << StartStop(startTick, endTick);
      }

void NoteList::dump(const QString& voice) const
      {
      // dump contents
      for (int i = 0; i < MAX_STAVES; ++i) {
            printf("voice %s staff %d:", qPrintable(voice), i);
            for (int j = 0; j < _staffNoteLists.at(i).size(); ++j)
                  printf(" %d-%d", _staffNoteLists.at(i).at(j).first, _staffNoteLists.at(i).at(j).second);
            printf("\n");
            }
      // show overlap
      printf("overlap voice %s:", qPrintable(voice));
      for (int i = 0; i < MAX_STAVES - 1; ++i)
            for (int j = i + 1; j < MAX_STAVES; ++j)
                  stavesOverlap(i, j);
      printf("\n");
      }

/**
 Determine if notes n1 and n2 overlap.
 This is NOT the case if
 - n1 starts when or after n2 stops
 - or n2 starts when or after n1 stops
 */

static bool notesOverlap(const StartStop& n1, const StartStop& n2)
      {
      return !(n1.first >= n2.second || n1.second <= n2.first);
      }

/**
 Determine if any note in staff1 and staff2 overlaps.
 */

bool NoteList::stavesOverlap(const int staff1, const int staff2) const
      {
      for (int i = 0; i < _staffNoteLists.at(staff1).size(); ++i)
            for (int j = 0; j < _staffNoteLists.at(staff2).size(); ++j)
                  if (notesOverlap(_staffNoteLists.at(staff1).at(i), _staffNoteLists.at(staff2).at(j))) {
                        // printf(" %d-%d", staff1, staff2);
                        return true;
                        }
      return false;
      }

/**
 Determine if any note in any staff overlaps.
 */

bool NoteList::anyStaffOverlaps() const
      {
      for (int i = 0; i < MAX_STAVES - 1; ++i)
            for (int j = i + 1; j < MAX_STAVES; ++j)
                  if (stavesOverlap(i, j))
                        return true;
      return false;
      }

VoiceOverlapDetector::VoiceOverlapDetector()
      {
      // qDebug("VoiceOverlapDetector::VoiceOverlapDetector(staves %d)", MAX_STAVES);
      }

void VoiceOverlapDetector::addNote(const int startTick, const int endTick, const QString& voice, const int staff)
      {
      // if necessary, create the note list for voice
      if (!_noteLists.contains(voice))
            _noteLists.insert(voice, NoteList());
      _noteLists[voice].addNote(startTick, endTick, staff);
      }

void VoiceOverlapDetector::dump() const
      {
      // qDebug("VoiceOverlapDetector::dump()");
      QMapIterator<QString, NoteList> i(_noteLists);
      while (i.hasNext()) {
            i.next();
            i.value().dump(i.key());
            }
      }

void VoiceOverlapDetector::newMeasure()
      {
      // qDebug("VoiceOverlapDetector::newMeasure()");
      _noteLists.clear();
      }

bool VoiceOverlapDetector::stavesOverlap(const QString& voice) const
      {
      if (_noteLists.contains(voice))
            return _noteLists.value(voice).anyStaffOverlaps();
      else
            return false;
      }

QString MusicXMLDrumInstrument::toString() const
      {
      return QString("pitch %1 name %2 notehead %3 line %4 stemDirection %5")
             .arg(pitch)
             .arg(name)
             .arg(int(notehead))
             .arg(line)
             .arg(int(stemDirection));
      }

void ValidatorMessageHandler::handleMessage(QtMsgType type, const QString& description,
                                            const QUrl& /* identifier */, const QSourceLocation& sourceLocation)
      {
      // convert description from html to text
      QDomDocument desc;
      QString contentError;
      int contentLine;
      int contentColumn;
      if (!desc.setContent(description, false, &contentError, &contentLine,
                           &contentColumn)) {
            qDebug("ValidatorMessageHandler: could not parse validation error line %d column %d: %s",
               contentLine, contentColumn, qPrintable(contentError));
            return;
            }
      QDomElement e = desc.documentElement();
      if (e.tagName() != "html") {
            qDebug("ValidatorMessageHandler: description is not html");
            return;
            }
      QString descText = e.text();

      QString strType;
      switch (type) {
            case 0:  strType = "Debug"; break;
            case 1:  strType = "Warning"; break;
            case 2:  strType = "Critical"; break;
            case 3:  strType = "Fatal"; break;
            default: strType = "Unknown"; break;
            }

      QString errorStr = QString("%1 error: line %2 column %3 %4")
            .arg(strType)
            .arg(sourceLocation.line())
            .arg(sourceLocation.column())
            .arg(descText);

      // append error, separated by newline if necessary
      if (errors != "")
            errors += "\n";
      errors += errorStr;
      }

//---------------------------------------------------------
//   printDomElementPath
//---------------------------------------------------------

static QString domElementPath(const QDomElement& e)
      {
      QString s;
      QDomNode dn(e);
      while (!dn.parentNode().isNull()) {
            dn = dn.parentNode();
            const QDomElement& e = dn.toElement();
            const QString k(e.tagName());
            if (!s.isEmpty())
                  s += ":";
            s += k;
            }
      return s;
      }

//---------------------------------------------------------
//   domError
//---------------------------------------------------------

void domError(const QDomElement& e)
      {
      QString m;
      QString s = domElementPath(e);
//      if (!docName.isEmpty())
//            m = QString("<%1>:").arg(docName);
      int ln = e.lineNumber();
      if (ln != -1)
            m += QString("line:%1 ").arg(ln);
      int col = e.columnNumber();
      if (col != -1)
            m += QString("col:%1 ").arg(col);
      m += QString("%1: Unknown Node <%2>, type %3").arg(s).arg(e.tagName()).arg(e.nodeType());
      if (e.isText())
            m += QString("  text node <%1>").arg(e.toText().data());
      qDebug("%s", qPrintable(m));
      }

//---------------------------------------------------------
//   domNotImplemented
//---------------------------------------------------------

void domNotImplemented(const QDomElement& e)
      {
      if (!MScore::debugMode)
            return;
      QString s = domElementPath(e);
//      if (!docName.isEmpty())
//            qDebug("<%s>:", qPrintable(docName));
      qDebug("%s: Node not implemented: <%s>, type %d",
             qPrintable(s), qPrintable(e.tagName()), e.nodeType());
      if (e.isText())
            qDebug("  text node <%s>", qPrintable(e.toText().data()));
      }


//---------------------------------------------------------
//   stringToInt
//---------------------------------------------------------

/**
 Convert a string in \a s into an int. Set *ok to true iff conversion was
 successful. \a s may end with ".0", as is generated by Audiveris 3.2 and up,
 in elements <divisions>, <duration>, <alter> and <sound> attributes
 dynamics and tempo.
 In case of error val return a default value of 0.
 Note that non-integer values cannot be handled by mscore.
 */

int MxmlSupport::stringToInt(const QString& s, bool* ok)
      {
      int res = 0;
      QString str = s;
      if (s.endsWith(".0"))
            str = s.left(s.size() - 2);
      res = str.toInt(ok);
      return res;
      }

//---------------------------------------------------------
//   durationAsFraction
//---------------------------------------------------------

/**
 Return duration specified in the element e as Fraction.
 Caller must ensure divisions is valid.
 */

Fraction MxmlSupport::durationAsFraction(const int divisions, const QDomElement e)
      {
      Fraction f;
      if (e.tagName() == "duration") {
            bool ok;
            int val = MxmlSupport::stringToInt(e.text(), &ok);
            f = Fraction(val, 4 * divisions); // note divisions = ticks / quarter note
            f.reduce();
            }
      else {
            qDebug() << "durationAsFraction tagname error" << f.print();
            }
      return f;
      }

//---------------------------------------------------------
//   noteTypeToFraction
//---------------------------------------------------------

/**
 Convert MusicXML note type to fraction.
 */

Fraction MxmlSupport::noteTypeToFraction(QString type)
      {
      if (type == "1024th")
            return Fraction(1, 1024);
      else if (type == "512th")
            return Fraction(1, 512);
      else if (type == "256th")
            return Fraction(1, 256);
      else if (type == "128th")
            return Fraction(1, 128);
      else if (type == "64th")
            return Fraction(1, 64);
      else if (type == "32nd")
            return Fraction(1, 32);
      else if (type == "16th")
            return Fraction(1, 16);
      else if (type == "eighth")
            return Fraction(1, 8);
      else if (type == "quarter")
            return Fraction(1, 4);
      else if (type == "half")
            return Fraction(1, 2);
      else if (type == "whole")
            return Fraction(1, 1);
      else if (type == "breve")
            return Fraction(2, 1);
      else if (type == "long")
            return Fraction(4, 1);
      else if (type == "maxima")
            return Fraction(8, 1);
      else
            return Fraction(0, 0);
      }

//---------------------------------------------------------
//   calculateFraction
//---------------------------------------------------------

/**
 Convert note type, number of dots and actual and normal notes into a duration
 */

Fraction MxmlSupport::calculateFraction(QString type, int dots, int normalNotes, int actualNotes)
      {
      // type
      Fraction f = MxmlSupport::noteTypeToFraction(type);
      if (f.isValid()) {
            // dot(s)
            Fraction f_no_dots = f;
            for (int i = 0; i < dots; ++i)
                  f += (f_no_dots / (2 << i));
            // tuplet
            if (actualNotes > 0 && normalNotes > 0) {
                  f *= normalNotes;
                  f /= actualNotes;
                  }
            // clean up (just in case)
            f.reduce();
            }
      return f;
      }

//---------------------------------------------------------
//   accSymId2MxmlString
//---------------------------------------------------------

QString accSymId2MxmlString(const SymId id)
      {
      QString s;
      switch (id) {
            case SymId::accidentalSharp:               s = "sharp";                break;
            case SymId::accidentalFlat:                s = "flat";                 break;
            case SymId::accidentalDoubleSharp:         s = "double-sharp";         break;
            case SymId::accidentalDoubleFlat:          s = "flat-flat";            break;
            case SymId::accidentalNatural:             s = "natural";              break;
                  /* TODO
            case Accidental::Type::FLAT_SLASH:         s = "slash-flat";           break;
            case Accidental::Type::MIRRORED_FLAT:      s = "quarter-flat";         break;
            case Accidental::Type::FLAT_ARROW_UP:      s = "flat-up";              break;
            case Accidental::Type::NATURAL_ARROW_DOWN: s = "natural-down";         break;
            case Accidental::Type::SHARP_SLASH:        s = "quarter-sharp";        break;
            case Accidental::Type::SHARP_ARROW_DOWN:   s = "sharp-down";           break;
            case Accidental::Type::NATURAL_ARROW_UP:   s = "natural-up";           break;
            case Accidental::Type::MIRRORED_FLAT2:     s = "three-quarters-flat";  break;
            case Accidental::Type::FLAT_FLAT_SLASH:    s = "three-quarters-flat";  break;
            case Accidental::Type::FLAT_ARROW_DOWN:    s = "flat-down";            break;
            case Accidental::Type::SHARP_SLASH4:       s = "three-quarters-sharp"; break;
            case Accidental::Type::SHARP_ARROW_UP:     s = "sharp-up";             break;
            case Accidental::Type::SHARP_SLASH3:       s = "slash-quarter-sharp";  break;
            case Accidental::Type::FLAT_SLASH2:        s = "double-slash-flat";    break;
            case Accidental::Type::SHARP_SLASH2:       s = "slash-sharp";          break;
            case Accidental::Type::SORI:               s = "sori";                 break;
            case Accidental::Type::KORON:              s = "koron";                break;
                   */
            default:
                  qDebug("accSymId2MxmlString: unknown accidental %d", static_cast<int>(id));
            }
      return s;
      }
      
//---------------------------------------------------------
//   accidentalType2MxmlString
//---------------------------------------------------------
      
QString accidentalType2MxmlString(const Accidental::Type type)
      {
      QString s;
      switch (type) {
            case Accidental::Type::SHARP:              s = "sharp";                break;
            case Accidental::Type::FLAT:               s = "flat";                 break;
            case Accidental::Type::SHARP2:             s = "double-sharp";         break;
            case Accidental::Type::FLAT2:              s = "flat-flat";            break;
            case Accidental::Type::NATURAL:            s = "natural";              break;
            case Accidental::Type::FLAT_SLASH:         s = "slash-flat";           break;
            case Accidental::Type::MIRRORED_FLAT:      s = "quarter-flat";         break;
            case Accidental::Type::FLAT_ARROW_UP:      s = "flat-up";              break;
            case Accidental::Type::NATURAL_ARROW_DOWN: s = "natural-down";         break;
            case Accidental::Type::SHARP_SLASH:        s = "quarter-sharp";        break;
            case Accidental::Type::SHARP_ARROW_DOWN:   s = "sharp-down";           break;
            case Accidental::Type::NATURAL_ARROW_UP:   s = "natural-up";           break;
            case Accidental::Type::MIRRORED_FLAT2:     s = "three-quarters-flat";  break;
            case Accidental::Type::FLAT_FLAT_SLASH:    s = "three-quarters-flat";  break;
            case Accidental::Type::FLAT_ARROW_DOWN:    s = "flat-down";            break;
            case Accidental::Type::SHARP_SLASH4:       s = "three-quarters-sharp"; break;
            case Accidental::Type::SHARP_ARROW_UP:     s = "sharp-up";             break;
            case Accidental::Type::SHARP_SLASH3:       s = "slash-quarter-sharp";  break;
            case Accidental::Type::FLAT_SLASH2:        s = "double-slash-flat";    break;
            case Accidental::Type::SHARP_SLASH2:       s = "slash-sharp";          break;
            case Accidental::Type::SORI:               s = "sori";                 break;
            case Accidental::Type::KORON:              s = "koron";                break;
            default:
                  qDebug("accidentalType2MxmlString: unknown accidental %d", static_cast<int>(type));
            }
      return s;
      }
      
//---------------------------------------------------------
//   mxmlString2accidentalType
//---------------------------------------------------------

/**
 Convert a MusicXML accidental name to a MuseScore enum Accidental::Type.
 */

Accidental::Type mxmlString2accidentalType(const QString mxmlName)
      {
      QMap<QString, Accidental::Type> map; // map MusicXML accidental name to MuseScore enum Accidental::Type
      map["natural"] = Accidental::Type::NATURAL;
      map["flat"] = Accidental::Type::FLAT;
      map["sharp"] = Accidental::Type::SHARP;
      map["double-sharp"] = Accidental::Type::SHARP2;
      map["sharp-sharp"] = Accidental::Type::SHARP2;
      map["flat-flat"] = Accidental::Type::FLAT2;
      map["double-flat"] = Accidental::Type::FLAT2;
      map["natural-flat"] = Accidental::Type::FLAT;
      
      map["quarter-flat"] = Accidental::Type::MIRRORED_FLAT;
      map["quarter-sharp"] = Accidental::Type::SHARP_SLASH;
      map["three-quarters-flat"] = Accidental::Type::MIRRORED_FLAT2;
      map["three-quarters-sharp"] = Accidental::Type::SHARP_SLASH4;
      
      map["sharp-down"] = Accidental::Type::SHARP_ARROW_DOWN;
      map["sharp-up"] = Accidental::Type::SHARP_ARROW_UP;
      map["natural-down"] = Accidental::Type::NATURAL_ARROW_DOWN;
      map["natural-up"] = Accidental::Type::NATURAL_ARROW_UP;
      map["flat-down"] = Accidental::Type::FLAT_ARROW_DOWN;
      map["flat-up"] = Accidental::Type::FLAT_ARROW_UP;
      
      map["slash-quarter-sharp"] = Accidental::Type::SHARP_SLASH3; // MIRRORED_FLAT_SLASH; ?
      map["slash-sharp"] = Accidental::Type::SHARP_SLASH2; // SHARP_SLASH; ?
      map["slash-flat"] = Accidental::Type::FLAT_SLASH;
      map["double-slash-flat"] = Accidental::Type::FLAT_SLASH2;
      
      map["sori"] = Accidental::Type::SORI;
      map["koron"] = Accidental::Type::KORON;
      
      map["natural-sharp"] = Accidental::Type::SHARP;
      
      if (map.contains(mxmlName))
            return map.value(mxmlName);
      else
            qDebug("mxmlString2accidentalTyp: unknown accidental %s", qPrintable(mxmlName));
      // default: return Accidental::Type::NONE
      return Accidental::Type::NONE;
      }

}
