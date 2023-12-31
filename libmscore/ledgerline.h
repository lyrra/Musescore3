//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2013 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#ifndef __LEDGERLINE_H__
#define __LEDGERLINE_H__

#include "element.h"

namespace Ms {

class Chord;

//---------------------------------------------------------
//    @@ LedgerLine
///     Graphic representation of a ledger line.
//!
//!    parent:     Chord
//!    x-origin:   Chord
//!    y-origin:   SStaff
//---------------------------------------------------------

class LedgerLine final : public Element {
      qreal _width;
      qreal _len;
      LedgerLine* _next;
      bool vertical { false };

   public:
      LedgerLine(Score*);
      LedgerLine &operator=(const LedgerLine&) = delete;

      LedgerLine* clone() const override { return new LedgerLine(*this); }
      ElementType type() const override  { return ElementType::LEDGER_LINE; }
      QPointF pagePos() const override;      ///< position in page coordinates
      Chord* chord() const                       { return toChord(parent()); }

      qreal len() const          { return _len;   }
      qreal lineWidth() const    { return _width; }
      void setLen(qreal v)       { _len = v;      }
      void setLineWidth(qreal v) { _width = v;    }

      void layout() override;
      void draw(QPainter*) const override;

      qreal measureXPos() const;
      LedgerLine* next() const    { return _next; }
      void setNext(LedgerLine* l) { _next = l;    }

      void writeProperties(XmlWriter& xml) const override;
      bool readProperties(XmlReader&) override;
      void spatiumChanged(qreal /*oldValue*/, qreal /*newValue*/) override;
      };

}     // namespace Ms
#endif

