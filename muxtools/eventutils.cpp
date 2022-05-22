//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2008-2011 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL
//=============================================================================

#include "libmscore/xml.h"
//#include "libmscore/note.h"
#include "libmscore/harmony.h"
//#include "libmscore/sig.h"
#include "event.h"
#include "eventutils.h"
#include "libmscore/staff.h"
#include "libmscore/instrument.h"
#include "libmscore/part.h"
#include "libmscore/score.h"

namespace Ms {

//---------------------------------------------------------
//   MidiCoreEvent::write
//---------------------------------------------------------

void midiCoreEvent_write(MidiCoreEvent ev, XmlWriter& xml)
      {
      uchar _type = ev.type();
      int _a = ev.dataA();
      int _b = ev.dataB();
      uchar _channel = ev.channel();
      switch(_type) {
            case ME_NOTEON:
                  xml.tagE(QString("note-on  channel=\"%1\" pitch=\"%2\" velo=\"%3\"")
                     .arg(_channel).arg(_a).arg(_b));
                  break;

            case ME_NOTEOFF:
                  xml.tagE(QString("note-off  channel=\"%1\" pitch=\"%2\" velo=\"%3\"")
                     .arg(_channel).arg(_a).arg(_b));
                  break;

            case ME_CONTROLLER:
                  if (_a == CTRL_PROGRAM) {
                        if (_channel == 0) {
                              xml.tagE(QString("program value=\"%1\"").arg(_b));
                              }
                        else {
                              xml.tagE(QString("program channel=\"%1\" value=\"%2\"")
                                 .arg(_channel).arg(_b));
                              }
                        }
                  else {
                        if (_channel == 0) {
                              xml.tagE(QString("controller ctrl=\"%1\" value=\"%2\"")
                                 .arg(_a).arg(_b));
                              }
                        else {
                              xml.tagE(QString("controller channel=\"%1\" ctrl=\"%2\" value=\"%3\"")
                                 .arg(_channel).arg(_a).arg(_b));
                              }
                        }
                  break;
            default:
                  qDebug("MidiCoreEvent::write: unknown type");
                  break;
            }
      }

//---------------------------------------------------------
//   Event::write
//---------------------------------------------------------

void event_write(Event ev, XmlWriter& xml)
      {
      uchar _type = ev.type();
      int _ontime = ev.ontime();
      uchar _channel = ev.channel();
      int _duration = ev.duration();
      int _a = ev.dataA();
      int _b = ev.dataB();
      const uchar* _edata = ev.edata();
      int _metaType = ev.metaType();
      int _len = ev.len();
      switch(_type) {
            case ME_NOTE:
                  xml.tagE(QString("note  tick=\"%1\" channel=\"%2\" len=\"%3\" pitch=\"%4\" velo=\"%5\"")
                     .arg(_ontime).arg(_channel).arg(_duration).arg(_a).arg(_b));
                  break;

            case ME_NOTEON:
                  xml.tagE(QString("note-on  tick=\"%1\" channel=\"%2\" pitch=\"%3\" velo=\"%4\"")
                     .arg(_ontime).arg(_channel).arg(_a).arg(_b));
                  break;

            case ME_NOTEOFF:
                  xml.tagE(QString("note-off  tick=\"%1\" channel=\"%2\" pitch=\"%3\" velo=\"%4\"")
                     .arg(_ontime).arg(_channel).arg(_a).arg(_b));
                  break;

            case ME_CONTROLLER:
                  if (_a == CTRL_PROGRAM) {
                        if ((_ontime == -1) && (_channel == 0)) {
                              xml.tagE(QString("program value=\"%1\"").arg(_b));
                              }
                        else {
                              xml.tagE(QString("program tick=\"%1\" channel=\"%2\" value=\"%3\"")
                                 .arg(_ontime).arg(_channel).arg(_b));
                              }
                        }
                  else {
                        if ((_ontime == -1) && (_channel == 0)) {
                              xml.tagE(QString("controller ctrl=\"%1\" value=\"%2\"")
                                 .arg(_a).arg(_b));
                              }
                        else {
                              xml.tagE(QString("controller tick=\"%1\" channel=\"%2\" ctrl=\"%3\" value=\"%4\"")
                                 .arg(_ontime).arg(_channel).arg(_a).arg(_b));
                              }
                        }
                  break;

            case ME_SYSEX:
                  xml.stag(QString("sysex tick=\"%1\" len=\"%2\"").arg(_ontime).arg(_len));
                  xml.dump(_len, _edata);
                  xml.etag();
                  break;

            case ME_META:
                  switch(_metaType) {
                        case META_TRACK_NAME:
                              xml.tag(QString("TrackName tick=\"%1\"").arg(_ontime), QString((char*)(_edata)));
                              break;

                        case META_LYRIC:
                              xml.tag(QString("Lyric tick=\"%1\"").arg(_ontime), QString((char*)(_edata)));
                              break;

                        case META_KEY_SIGNATURE:
                              {
                              const char* keyTable[] = {
                                    "Ces", "Ges", "Des", "As", "Es", "Bes", "F",
                                    "C",
                                    "G", "D", "A", "E", "B", "Fis", "Cis"
                                    };
                              int key = (char)(_edata[0]) + 7;
                              if (key < 0 || key > 14) {
                                    qDebug("bad key signature %d", key);
                                    key = 0;
                                    }
                              QString sex(_edata[1] ? "Minor" : "Major");
                              QString keyName(keyTable[key]);
                              xml.tag(QString("Key tick=\"%1\" key=\"%2\" sex=\"%3\"").arg(_ontime).arg(_edata[0]).arg(_edata[1]),
                                 QString("%1 %2").arg(keyName, sex));
                              }
                              break;

                        case META_TIME_SIGNATURE:
                              xml.tagE(QString("TimeSig tick=\"%1\" num=\"%2\" denom=\"%3\" metro=\"%4\" quarter=\"%5\"")
                                 .arg(_ontime)
                                 .arg(int(_edata[0]))
                                 .arg(int(_edata[1]))
                                 .arg(int(_edata[2]))
                                 .arg(int(_edata[3])));
                              break;

                        case META_TEMPO:
                              {
                              unsigned tempo = _edata[2] + (_edata[1] << 8) + (_edata[0] << 16);
                              xml.tagE(QString("Tempo tick=\"%1\" value=\"%2\"").arg(_ontime).arg(tempo));
                              }
                              break;

                        default:
                              xml.stag(QString("Meta tick=\"%1\" type=\"%2\" len=\"%3\" name=\"%4\"")
                                 .arg(_ontime).arg(_metaType).arg(_len).arg(midiMetaName(_metaType)));
                              xml.dump(_len, _edata);
                              xml.etag();
                              break;
                        }
                  break;
            }
      }

//---------------------------------------------------------
//   isMuted
//---------------------------------------------------------

bool nPlayEvent_isMuted(NPlayEvent ev)
      {
      const Note* n = ev.note();
      if (n) {
            MasterScore* cs = n->masterScore();
            Staff* staff = n->staff();
            Instrument* instr = staff->part()->instrument(n->tick());
            const Channel* a = instr->playbackChannel(n->subchannel(), cs);
            return a->mute() || a->soloMute() || !staff->playbackVoice(n->voice());
            }

      const Harmony* h = ev.harmony();
      if (h) {
            const Channel* hCh = h->part()->harmonyChannel();
            if (hCh) { //if there is a harmony channel
                  const Channel* pCh = h->masterScore()->playbackChannel(hCh);
                  return pCh->mute() || pCh->soloMute();
                  }
            }

      return false;
      }

//---------------------------------------------------------
// insert
//---------------------------------------------------------

void EventList::insert(const Event& e)
      {
      int ontime = e.ontime();
      if (!isEmpty() && last().ontime() > ontime) {
            for (auto i = begin(); i != end(); ++i) {
                  if (i->ontime() > ontime) {
                        QList<Event>::insert(i, e);
                        return;
                        }
                  }
            }
      append(e);
      }

//---------------------------------------------------------
//   class EventMap::fixupMIDI
//---------------------------------------------------------
void eventMap_fixupMIDI(EventMap& em)
      {
      /* track info for each of the 128 possible MIDI notes */
      struct channelInfo {
            /* which event the first ME_NOTEON came from */
            NPlayEvent *event[128];
            /* how often is the note on right now? */
            unsigned short nowPlaying[128];
            };

      /* track info for each channel (on the heap, 0-initialised) */
      struct channelInfo *info = (struct channelInfo *)calloc(em.highestChannel() + 1, sizeof(struct channelInfo));

      auto it = em.begin();
      while (it != em.end()) {
            NPlayEvent& event = it->second;
            /* ME_NOTEOFF is never emitted, no need to check for it */
            if (event.type() == ME_NOTEON && !nPlayEvent_isMuted(event)) {
                  unsigned short np = info[event.channel()].nowPlaying[event.pitch()];
                  if (event.velo() == 0) {
                        /* already off (should not happen) or still playing? */
                        if (np == 0 || --np > 0)
                              event.setDiscard(1);
                        else {
                              /* hoist NOTEOFF to same track as NOTEON */
                              event.setOriginatingStaff(info[event.channel()].event[event.pitch()]->getOriginatingStaff());
                              }
                        }
                  else {
                        if (++np > 1)
                              /* restrike, possibly on different track */
                              event.setDiscard(info[event.channel()].event[event.pitch()]->getOriginatingStaff() + 1);
                        info[event.channel()].event[event.pitch()] = &event;
                        }
                  info[event.channel()].nowPlaying[event.pitch()] = np;
                  }

            ++it;
            }

            free((void *)info);
      }


} // Namespace Ms
