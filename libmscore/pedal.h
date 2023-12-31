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

#ifndef __PEDAL_H__
#define __PEDAL_H__

#include "textlinebase.h"

namespace Ms {

class Pedal;

//---------------------------------------------------------
//   @@ PedalSegment
//---------------------------------------------------------

class PedalSegment final : public TextLineBaseSegment {

      Sid getPropertyStyle(Pid) const override;

   public:
      PedalSegment(Spanner* sp, Score* s) : TextLineBaseSegment(sp, s, ElementFlag::MOVABLE | ElementFlag::ON_STAFF) {}

      ElementType type() const override       { return ElementType::PEDAL_SEGMENT; }
      PedalSegment* clone() const override    { return new PedalSegment(*this);    }
      Pedal* pedal() const                    { return toPedal(spanner());          }
      void layout() override;

      friend class Pedal;
      };

//---------------------------------------------------------
//   @@ Pedal
//---------------------------------------------------------

class Pedal final : public TextLineBase {

      Sid getPropertyStyle(Pid) const override;

   protected:
      QPointF linePos(Grip, System**) const override;

   public:
      Pedal(Score* s);

      Pedal* clone() const override     { return new Pedal(*this);   }
      ElementType type() const override { return ElementType::PEDAL; }

      void read(XmlReader&) override;
      void write(XmlWriter& xml) const override;

      LineSegment* createLineSegment() override;
      QVariant propertyDefault(Pid propertyId) const override;

      friend class PedalLine;
      };

}     // namespace Ms
#endif

