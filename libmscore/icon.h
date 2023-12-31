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

#ifndef __ICON_H__
#define __ICON_H__

#include "element.h"
#include "mscore.h"

namespace Ms {

//---------------------------------------------------------
//   Icon
//    dummy element, used for drag&drop
//---------------------------------------------------------

class Icon final : public Element {
      IconType _iconType { IconType::NONE };
      QByteArray _action;
      QIcon _icon;
      int _extent { 40 };

   public:
      Icon(Score* s) : Element(s) { }
      virtual ~Icon() {}

      Icon* clone() const override                { return new Icon(*this);    }
      ElementType type() const override           { return ElementType::ICON;  }
      IconType iconType() const                           { return _iconType;          }
      void setIconType(IconType val)                      { _iconType = val;           }
      void setAction(const QByteArray& a, const QIcon& i) { _action = a; _icon = i; }
      const QByteArray& action() const                    { return _action; }
      QIcon icon() const                                  { return _icon;   }
      void setExtent(int v)                               { _extent = v; }
      int extent() const                                  { return _extent; }
      void write(XmlWriter&) const override;
      void read(XmlReader&) override;
      void draw(QPainter*) const override;
      void layout() override;

      QVariant getProperty(Pid) const override;
      bool setProperty(Pid, const QVariant&) override;
      };


}     // namespace Ms
#endif

