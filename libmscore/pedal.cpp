//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2002-2011 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL
//=============================================================================

#include "pedal.h"
#include "sym.h"
#include "xml.h"
#include "system.h"
#include "measure.h"
#include "chordrest.h"
#include "staff.h"

#include "score.h"

namespace Ms {

//---------------------------------------------------------
//   layout
//---------------------------------------------------------

void PedalSegment::layout()
      {
      if (autoplace())
            setUserOff(QPointF());
      TextLineBaseSegment::layout();
      if (parent()) {     // for palette
            if (pedal()->placeBelow())
                  rypos() += score()->styleP(StyleIdx::pedalPosBelow) + (staff() ? staff()->height() : 0.0);
            else
                  rypos() += score()->styleP(StyleIdx::pedalPosAbove);
            if (autoplace()) {
                  qreal minDistance = spatium() * .7;
                  Shape s1 = shape().translated(pos());

                  if (pedal()->placeBelow()) {
                        qreal d  = system()->bottomDistance(staffIdx(), s1);
                        if (d > -minDistance)
                              rUserYoffset() = d + minDistance;
                        }
                  else {
                        qreal d  = system()->topDistance(staffIdx(), s1);
                        if (d > -minDistance)
                              rUserYoffset() = -(d + minDistance);
                        }
                  }
            else
                  adjustReadPos();
            }
      }


//---------------------------------------------------------
//   Pedal
//---------------------------------------------------------

Pedal::Pedal(Score* s)
   : TextLineBase(s)
      {
      setLineVisible(true);
      resetProperty(P_ID::BEGIN_TEXT);
      resetProperty(P_ID::END_TEXT);

      resetProperty(P_ID::LINE_WIDTH);
      resetProperty(P_ID::LINE_STYLE);

      resetProperty(P_ID::BEGIN_HOOK_TYPE);
      resetProperty(P_ID::END_HOOK_TYPE);

      resetProperty(P_ID::BEGIN_TEXT_PLACE);

      initSubStyle(SubStyleId::PEDAL);
      }

//---------------------------------------------------------
//   read
//---------------------------------------------------------

void Pedal::read(XmlReader& e)
      {
      int id = e.intAttribute("id", -1);
      e.addSpanner(id, this);
      while (e.readNextStartElement()) {
            if (!TextLineBase::readProperties(e))
                  e.unknown();
            }
      }

//---------------------------------------------------------
//   write
//---------------------------------------------------------

void Pedal::write(XmlWriter& xml) const
      {
      if (!xml.canWrite(this))
            return;
      int id = xml.spannerId(this);
      xml.stag(QString("%1 id=\"%2\"").arg(name()).arg(id));

      for (auto i : {
         P_ID::END_HOOK_TYPE,
         P_ID::BEGIN_TEXT,
         P_ID::END_TEXT,
         P_ID::LINE_WIDTH,
         P_ID::LINE_STYLE,
         P_ID::BEGIN_HOOK_TYPE
         }) {
            writeProperty(xml, i);
            }
      for (const StyledProperty* spp = styledProperties(); spp->styleIdx != StyleIdx::NOSTYLE; ++spp)
            writeProperty(xml, spp->propertyIdx);

      Element::writeProperties(xml);
      xml.etag();
      }


//---------------------------------------------------------
//   createLineSegment
//---------------------------------------------------------

LineSegment* Pedal::createLineSegment()
      {
      return new PedalSegment(score());
      }

//---------------------------------------------------------
//   setYoff
//---------------------------------------------------------

void Pedal::setYoff(qreal val)
      {
      rUserYoffset() += val * spatium() - score()->styleP(placeAbove() ? StyleIdx::pedalPosAbove : StyleIdx::pedalPosBelow);
      }

//---------------------------------------------------------
//   propertyDefault
//---------------------------------------------------------

QVariant Pedal::propertyDefault(P_ID propertyId) const
      {
      switch (propertyId) {
            case P_ID::LINE_WIDTH:
                  return score()->styleV(StyleIdx::pedalLineWidth);

            case P_ID::LINE_STYLE:
                  return score()->styleV(StyleIdx::pedalLineStyle);

            case P_ID::BEGIN_TEXT:
            case P_ID::END_TEXT:
                  return "";

            case P_ID::BEGIN_TEXT_PLACE:
                  return int(PlaceText::LEFT);

            case P_ID::BEGIN_HOOK_TYPE:
            case P_ID::END_HOOK_TYPE:
                  return int(HookType::NONE);

            default:
                  return TextLineBase::propertyDefault(propertyId);
            }
      }

//---------------------------------------------------------
//   linePos
//    return System() coordinates
//---------------------------------------------------------

QPointF Pedal::linePos(Grip grip, System** sys) const
      {
      qreal x;
      qreal nhw = score()->noteHeadWidth();
      System* s = nullptr;
      if (grip == Grip::START) {
            ChordRest* c = toChordRest(startElement());
            s = c->segment()->system();
            x = c->pos().x() + c->segment()->pos().x() + c->segment()->measure()->pos().x();
            if (c->type() == ElementType::REST && c->durationType() == TDuration::DurationType::V_MEASURE)
                  x -= c->x();
            if (beginHookType() == HookType::HOOK_45)
                  x += nhw * .5;
            }
      else {
            Element* e = endElement();
            ChordRest* c = toChordRest(endElement());
            if (!e || e == startElement() || (endHookType() == HookType::HOOK_90)) {
                  // pedal marking on single note or ends with non-angled hook:
                  // extend to next note or end of measure
                  Segment* seg = nullptr;
                  if (!e)
                        seg = startSegment();
                  else
                        seg = c->segment();
                  if (seg) {
                        seg = seg->next();
                        for ( ; seg; seg = seg->next()) {
                              if (seg->segmentType() == SegmentType::ChordRest) {
                                    // look for a chord/rest in any voice on this staff
                                    bool crFound = false;
                                    int track = staffIdx() * VOICES;
                                    for (int i = 0; i < VOICES; ++i) {
                                          if (seg->element(track + i)) {
                                                crFound = true;
                                                break;
                                                }
                                          }
                                    if (crFound)
                                          break;
                                    }
                              else if (seg->segmentType() == SegmentType::EndBarLine) {
                                    break;
                                    }
                              }
                        }
                  if (seg) {
                        s = seg->system();
                        x = seg->pos().x() + seg->measure()->pos().x() - nhw * 2;
                        }
                  }
            else if (c) {
                  s = c->segment()->system();
                  x = c->pos().x() + c->segment()->pos().x() + c->segment()->measure()->pos().x();
                  if (c->type() == ElementType::REST && c->durationType() == TDuration::DurationType::V_MEASURE)
                        x -= c->x();
                  }
            if (!s) {
                  int t = tick2();
                  Measure* m = score()->tick2measure(t);
                  s = m->system();
                  x = m->tick2pos(t);
                  }
            if (endHookType() == HookType::HOOK_45)
                  x += nhw * .5;
            else
                  x += nhw;
            }

      *sys = s;
      return QPointF(x, 0);
      }

}

