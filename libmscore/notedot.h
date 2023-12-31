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

#ifndef __NOTEDOT_H__
#define __NOTEDOT_H__

#include "element.h"

namespace Ms {

class Note;
class Rest;

//---------------------------------------------------------
//   @@ NoteDot
//---------------------------------------------------------

class NoteDot final : public Element {

   public:
      NoteDot(Score* = 0);

      NoteDot* clone() const override     { return new NoteDot(*this); }
      ElementType type() const override   { return ElementType::NOTEDOT; }
      qreal mag() const override;

      void draw(QPainter*) const override;
      void read(XmlReader&) override;
      void layout() override;

      Note* note() const { return parent()->isNote() ? toNote(parent()) : 0; }
      Rest* rest() const { return parent()->isRest() ? toRest(parent()) : 0; }
      };


}     // namespace Ms
#endif

