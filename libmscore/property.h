//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2011 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#ifndef __PROPERTY_H__
#define __PROPERTY_H__

namespace Ms {

class XmlReader;

//------------------------------------------------------------------------
//    M_PROPERTY (type, getter_name, setter_name)
//       helper macro to define a styled ScoreElement property
//
//    usage example:
//    class Text : public Element {
//          M_PROPERTY(QColor, color, setColor)
//          ...
//          };
//    this defines:
//          QColor _color;
//          const QColor& color() const { return _color; }
//          void setColor(const QColor& val) { _color = val; }
//---------------------------------------------------------

#define M_PROPERTY(a,b,c)                                      \
      a _ ## b;                                                \
   public:                                                     \
      const a& b() const   { return _ ## b; }                  \
      void c(const a& val) { _ ## b = val;  }                  \
   private:

#define M_PROPERTY2(a,b,c,d)                                   \
      a _ ## b { d };                                          \
   public:                                                     \
      const a& b() const   { return _ ## b; }                  \
      void c(const a& val) { _ ## b = val;  }                  \
   private:

#include "property-gen.h"

extern QVariant readProperty(Pid type, XmlReader& e);
extern QVariant propertyFromString(Pid type, QString value);
extern QString propertyToString(Pid, QVariant value, bool mscx);
extern P_TYPE propertyType(Pid);
extern const char* propertyName(Pid);
extern bool propertyLink(Pid id);
extern Pid propertyId(const QString& name);
extern Pid propertyId(const QStringRef& name);
extern QString propertyUserName(Pid);

}     // namespace Ms

Q_DECLARE_METATYPE(QPainterPath); // for properties with P_TYPE::PATH

#endif

