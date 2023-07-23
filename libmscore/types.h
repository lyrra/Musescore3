//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2017 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL
//=============================================================================

#ifndef __TYPES_H__
#define __TYPES_H__

#include "config.h"

/**
 * \namespace Ms .
 */

namespace Ms {
#ifdef SCRIPT_INTERFACE
Q_NAMESPACE
#endif


#include "types-gen.h"

constexpr NoteType operator| (NoteType t1, NoteType t2) {
      return static_cast<NoteType>(static_cast<int>(t1) | static_cast<int>(t2));
      }
constexpr bool operator& (NoteType t1, NoteType t2) {
      return static_cast<int>(t1) & static_cast<int>(t2);
      }

constexpr SegmentType operator| (const SegmentType t1, const SegmentType t2) {
      return static_cast<SegmentType>(static_cast<int>(t1) | static_cast<int>(t2));
      }
constexpr bool operator& (const SegmentType t1, const SegmentType t2) {
      return static_cast<int>(t1) & static_cast<int>(t2);
      }

constexpr Align operator| (Align a1, Align a2) {
      return static_cast<Align>(static_cast<char>(a1) | static_cast<char>(a2));
      }
constexpr bool operator& (Align a1, Align a2) {
      return static_cast<char>(a1) & static_cast<char>(a2);
      }
constexpr Align operator~ (Align a) {
      return static_cast<Align>(~static_cast<char>(a));
      }

constexpr FontStyle operator+ (FontStyle a1, FontStyle a2) {
      return static_cast<FontStyle>(static_cast<char>(a1) | static_cast<char>(a2));
      }
constexpr FontStyle operator- (FontStyle a1, FontStyle a2) {
      return static_cast<FontStyle>(static_cast<char>(a1) & ~static_cast<char>(a2));
      }
constexpr bool operator& (FontStyle a1, FontStyle a2) {
      return static_cast<bool>(static_cast<char>(a1) & static_cast<char>(a2));
      }

#ifdef SCRIPT_INTERFACE
Q_ENUM_NS(ElementType);
Q_ENUM_NS(Direction);
Q_ENUM_NS(GlissandoType);
Q_ENUM_NS(GlissandoStyle);
Q_ENUM_NS(Placement);
Q_ENUM_NS(HPlacement);
Q_ENUM_NS(SegmentType);
Q_ENUM_NS(Tid);
Q_ENUM_NS(Align);
Q_ENUM_NS(NoteType);
Q_ENUM_NS(PlayEventType);
Q_ENUM_NS(AccidentalType);
Q_ENUM_NS(HarmonyType);
#endif

//hack: to force the build system to run moc on this file
/// \private
class Mops : public QObject {
      Q_GADGET
      };

extern Direction toDirection(const QString&);
extern const char* toString(Direction);
extern QString toUserString(Direction);
extern void fillComboBoxDirection(QComboBox*);


} // namespace Ms

Q_DECLARE_METATYPE(Ms::Align);

Q_DECLARE_METATYPE(Ms::Direction);

Q_DECLARE_METATYPE(Ms::NoteType);

Q_DECLARE_METATYPE(Ms::PlayEventType);

Q_DECLARE_METATYPE(Ms::AccidentalType);

Q_DECLARE_METATYPE(Ms::HPlacement);

#endif
