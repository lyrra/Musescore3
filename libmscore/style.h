//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2002-2011 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#ifndef __STYLE_H__
#define __STYLE_H__

#include "chordlist.h"
#include "types.h"

namespace Ms {

enum class Pid : int;
class XmlWriter;
struct ChordDescription;
class Element;
class Score;

// Needs to be duplicated here and in sym.h since moc doesn't handle macros from #include'd files
#ifdef SCRIPT_INTERFACE
#define BEGIN_QT_REGISTERED_ENUM(Name) \
class MSQE_##Name { \
      Q_GADGET \
   public:
#define END_QT_REGISTERED_ENUM(Name) \
      Q_ENUM(Name); \
      }; \
using Name = MSQE_##Name::Name;
#else
#define BEGIN_QT_REGISTERED_ENUM(Name)
#define END_QT_REGISTERED_ENUM(Name)
#endif

//---------------------------------------------------------
//   Sid
///   Enumerates the list of score style settings
//
//    Keep in sync with styleTypes[] in style.cpp
//---------------------------------------------------------

BEGIN_QT_REGISTERED_ENUM(Sid)
#include "style-gen.h"
END_QT_REGISTERED_ENUM(Sid)

inline uint qHash(Sid id)
{
    return static_cast<uint>(id);
}

//---------------------------------------------------------
//   MStyle
///   \cond PLUGIN_API \private \endcond
//    the name "Style" gives problems with some microsoft
//    header files...
//---------------------------------------------------------

class MStyle {
      std::array<QVariant, int(Sid::STYLES)> _values;
      std::array<qreal, int(Sid::STYLES)> _precomputedValues;

      ChordList _chordList;
      bool _customChordList;        // if true, chordlist will be saved as part of score
      int _defaultStyleVersion = -1;

   public:
      MStyle();

      void precomputeValues();
      const QVariant& value(Sid idx) const;
      qreal pvalue(Sid idx) const    { return _precomputedValues[int(idx)]; }
      void set(Sid idx, const QVariant& v);

      bool isDefault(Sid idx) const;
      void setDefaultStyleVersion(const int defaultsVersion);
      int defaultStyleVersion() const { return _defaultStyleVersion; }

      const ChordDescription* chordDescription(int id) const;
      ChordList* chordList()  { return &_chordList; }
      void setChordList(ChordList*, bool custom = true);    // Style gets ownership of ChordList
      void setCustomChordList(bool t) { _customChordList = t; }
      void checkChordList();

      bool load(QFile* qf, bool ign = false);
      void load(XmlReader& e);
      void applyNewDefaults(const MStyle& other, const int defaultsVersion);
      void save(XmlWriter& xml, bool optimize);
      bool readProperties(XmlReader&);
      bool readStyleValCompat(XmlReader&);
      bool readTextStyleValCompat(XmlReader&);

      void resetAllStyles(Score* score, const QSet<Sid>& ignoredStyles = QSet<Sid>());
      void resetStyles(Score* score, const QSet<Sid>& stylesToReset);

      static const char* valueType(const Sid);
      static const char* valueName(const Sid);
      static Sid styleIdx(const QString& name);
      static MStyle* resolveStyleDefaults(const int defaultsVersion);
      };

//---------------------------------------------------------
//   StyledProperty
///   \cond PLUGIN_API \private \endcond
//---------------------------------------------------------

struct StyledProperty {
      Sid sid;
      Pid pid;
      };

typedef std::vector<StyledProperty> ElementStyle;

#define TEXT_STYLE_SIZE 14

typedef std::array<StyledProperty, TEXT_STYLE_SIZE> TextStyle;


const TextStyle* textStyle(Tid);
const TextStyle* textStyle(const char*);

const char* textStyleName(Tid);
QString textStyleUserName(Tid);
Tid textStyleFromName(const QString&);

const std::vector<Tid>& allTextStyles();
const std::vector<Tid>& primaryTextStyles();

QSet<Sid> pageStyles();
QSet<Sid> fretStyles();

#ifndef NDEBUG
extern void checkStyles();
#endif

}     // namespace Ms


#endif
