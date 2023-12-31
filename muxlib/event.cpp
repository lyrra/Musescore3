//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2008-2011 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL2
//=============================================================================

#include "mux-qt.h"
#include "musescore-qt.h"
#include "libmscore/sig.h"
#include "event.h"

namespace Ms {

//---------------------------------------------------------
//   Event::Event
//---------------------------------------------------------

Event::Event()
      {
      _type            = 0;
      _ontime          = -1;
      _noquantOntime   = 0;
      _noquantDuration = 0;
      _channel         = 0;
      _a               = 0;
      _b               = 0;
      _duration        = 0;
      _tpc             = 0;
      _voice           = 0;
      _edata           = 0;
      _len             = 0;
      _metaType        = 0;
      _note            = 0;
      _tuning          = 0.0;
      }

Event::Event(int t)
      {
      _type            = t;
      _ontime          = -1;
      _noquantOntime   = 0;
      _noquantDuration = 0;
      _channel         = 0;
      _a               = 0;
      _b               = 0;
      _duration        = 0;
      _tpc             = 0;
      _voice           = 0;
      _edata            = 0;
      _len             = 0;
      _metaType        = 0;
      _note            = 0;
      _tuning          = 0.0;
      }

Event::Event(const Event& e)
   : PlayEvent(e)
      {
      _type       = e._type;
      _ontime     = e._ontime;
      _noquantOntime   = e._noquantOntime;
      _noquantDuration = e._noquantDuration;
      _channel    = e._channel;
      _a          = e._a;
      _b          = e._b;
      _duration   = e._duration;
      _tpc        = e._tpc;
      _voice      = e._voice;
      _notes      = e._notes;
      if (e._edata) {
            _edata = new unsigned char[e._len + 1];  // don’t forget trailing zero
            memcpy(_edata, e._edata, e._len+1);
            }
      else
            _edata = 0;
      _len        = e._len;
      _metaType   = e._metaType;
      _note       = e._note;
      _tuning     = e._tuning;
      }

Event::~Event()
      {
      delete[] _edata;
      }

//---------------------------------------------------------
//   NPlayEvent::NPlayEvent (beatType2metronomeEvent)
//---------------------------------------------------------

NPlayEvent::NPlayEvent(BeatType beatType)
      {
      setType(ME_TICK2);
      setVelo(127);
      switch (beatType) {
            case BeatType::DOWNBEAT:
                  setType(ME_TICK1);
                  break;
            case BeatType::SIMPLE_STRESSED:
            case BeatType::COMPOUND_STRESSED:
                  // use defaults
                  break;
            case BeatType::SIMPLE_UNSTRESSED:
            case BeatType::COMPOUND_UNSTRESSED:
                  setVelo(80);
                  break;
            case BeatType::COMPOUND_SUBBEAT:
                  setVelo(25);
                  break;
            case BeatType::SUBBEAT:
                  setVelo(15);
                  break;
            }
      }

//---------------------------------------------------------
//   isMuted
//---------------------------------------------------------

bool NPlayEvent::isMuted() const
      {
      return false;
// FIX: set this at musescore-side
#if 0
      const Note* n = note();
      if (n) {
            MasterScore* cs = n->masterScore();
            Staff* staff = n->staff();
            Instrument* instr = staff->part()->instrument(n->tick());
            const Channel* a = instr->playbackChannel(n->subchannel(), cs);
            return a->mute() || a->soloMute() || !staff->playbackVoice(n->voice());
            }

      const Harmony* h = harmony();
      if (h) {
            const Channel* hCh = h->part()->harmonyChannel();
            if (hCh) { //if there is a harmony channel
                  const Channel* pCh = h->masterScore()->playbackChannel(hCh);
                  return pCh->mute() || pCh->soloMute();
                  }
            }

      return false;
      #endif
      }

//---------------------------------------------------------
//   dump
//---------------------------------------------------------

void Event::dump() const
      {
      printf("event ");
      switch (_type) {
            case ME_NOTEON:     printf("noteon    "); break;
            case ME_CONTROLLER: printf("controller"); break;
            case ME_PROGRAM:    printf("program   "); break;
            default:            printf("0x%02x    ", _type); break;
            }
      printf(" 0x%02x 0x%02x\n", _a, _b);
      }

//---------------------------------------------------------
//   isChannelEvent
//---------------------------------------------------------

bool MidiCoreEvent::isChannelEvent() const
      {
      switch(_type) {
            case ME_NOTEOFF:
            case ME_NOTEON:
            case ME_POLYAFTER:
            case ME_CONTROLLER:
            case ME_PROGRAM:
            case ME_AFTERTOUCH:
            case ME_PITCHBEND:
            case ME_NOTE:
            case ME_CHORD:
                  return true;
            default:
                  return false;
            }

      // Prevent "unreachable code" warning.
      // return false;
      }

bool Event::operator==(const Event&) const
      {
      return false;           // TODO
      }

//---------------------------------------------------------
//    midi_meta_name
//---------------------------------------------------------

QString midiMetaName(int meta)
      {
      const char* s = "";
      switch (meta) {
            case 0:     s = "Sequence Number"; break;
            case 1:     s = "Text Event"; break;
            case 2:     s = "Copyright"; break;
            case 3:     s = "Sequence/Track Name"; break;
            case 4:     s = "Instrument Name"; break;
            case 5:     s = "Lyric"; break;
            case 6:     s = "Marker"; break;
            case 7:     s = "Cue Point"; break;
            case 8:
            case 9:
            case 0x0a:
            case 0x0b:
            case 0x0c:
            case 0x0d:
            case 0x0e:
            case 0x0f:  s = "Text"; break;
            case 0x20:  s = "Channel Prefix"; break;
            case 0x21:  s = "Port Change"; break;
            case 0x2f:  s = "End of Track"; break;
            case META_TEMPO:  s = "Tempo"; break;
            case 0x54:  s = "SMPTE Offset"; break;
            case META_TIME_SIGNATURE:  s = "Time Signature"; break;
            case META_KEY_SIGNATURE:   s = "Key Signature"; break;
            case 0x74:                 s = "Sequencer-Specific1"; break;
            case 0x7f:                 s = "Sequencer-Specific2"; break;
            default:
                  break;
            }
      return QString(s);
      }


}
