//=============================================================================
//  MuseScore
//  Linux Music Score Editor
//
//  Copyright (C) 2002-2016 Werner Schweer and others
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//=============================================================================

#include <QApplication>
#include <QPushButton>
#include <QXmlStreamReader>
#include <QtMath>
#include "musescore-gui-qt.h"
#include "mixertrackitem.h"

#include "libmscore/score.h"
#include "libmscore/part.h"
#include "muxcommon.h"
#include "muxseq_client.h"

namespace Ms {

//---------------------------------------------------------
//   MixerTrackItem
//---------------------------------------------------------

MixerTrackItem::MixerTrackItem(TrackType tt, Part* part, Instrument* instr, Channel *chan)
      :_trackType(tt), _part(part), _instr(instr), _chan(chan)
      {
      }

//---------------------------------------------------------
//   midiMap
//---------------------------------------------------------

MidiMapping *MixerTrackItem::midiMap()
      {
      return _part->masterScore()->midiMapping(focusedChan()->channel());
      }

//---------------------------------------------------------
//   partChan
//---------------------------------------------------------

Channel *MixerTrackItem::focusedChan()
      {
      return _chan;
      }

//---------------------------------------------------------
//   playbackChannel
//---------------------------------------------------------

Channel* MixerTrackItem::playbackChannel(const Channel* channel)
      {
      return _part->masterScore()->playbackChannel(channel);
      }

//---------------------------------------------------------
//   color
//---------------------------------------------------------

int MixerTrackItem::color()
      {
            return _trackType ==TrackType::PART ? _part->color()
                                                : _chan->color();
      }

//---------------------------------------------------------
//   setVolume
//---------------------------------------------------------

void MixerTrackItem::setVolume(char value)
      {
//      char v = (char)qBound(0, static_cast<int>(value * 128.0 / 100.0), 127);

      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        if (chan->volume() != value) {
                              chan->setVolume(value);
                              muxseq_seq_setController(chan->channel(), CTRL_VOLUME, chan->volume());
                              }
                        }
                  }
            }
      else {
            if (_chan->volume() != value) {
                  _chan->setVolume(value);
                  muxseq_seq_setController(_chan->channel(), CTRL_VOLUME, _chan->volume());
                  }
            }
      }

//---------------------------------------------------------
//   setPan
//---------------------------------------------------------

void MixerTrackItem::setPan(char value)
      {
//      char v = (char)qBound(0, static_cast<int>((value + 180.0) / 360.0 * 128.0), 127);

      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        if (chan->pan() != value) {
                              chan->setPan(value);
                              muxseq_seq_setController(chan->channel(), CTRL_PANPOT, chan->pan());
                              }
                        }
                  }
            }
      else {
            if (_chan->pan() != value) {
                  _chan->setPan(value);
                  muxseq_seq_setController(_chan->channel(), CTRL_PANPOT, _chan->pan());
                  }
            }
      }

//---------------------------------------------------------
//   setChorus
//---------------------------------------------------------

void MixerTrackItem::setChorus(char value)
      {
//      char v = (char)qBound(0, static_cast<int>(value * 128.0 / 100.0), 127);

      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        if (chan->chorus() != value) {
                              chan->setChorus(value);
                              muxseq_seq_setController(chan->channel(), CTRL_CHORUS_SEND, chan->chorus());
                              }
                        }
                  }
            }
      else {
            if (_chan->chorus() != value) {
                  _chan->setChorus(value);
                  muxseq_seq_setController(_chan->channel(), CTRL_CHORUS_SEND, _chan->chorus());
                  }
            }
      }

//---------------------------------------------------------
//   setReverb
//---------------------------------------------------------

void MixerTrackItem::setReverb(char value)
      {
//      char v = (char)qBound(0, static_cast<int>(value * 128.0 / 100.0), 127);

      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        if (chan->reverb() != value) {
                              chan->setReverb(value);
                              muxseq_seq_setController(chan->channel(), CTRL_REVERB_SEND, chan->reverb());
                              }
                        }
                  }
            }
      else {
            if (_chan->reverb() != value) {
                  _chan->setReverb(value);
                  muxseq_seq_setController(_chan->channel(), CTRL_REVERB_SEND, _chan->reverb());
                  }
            }
      }

//---------------------------------------------------------
//   setColor
//---------------------------------------------------------

void MixerTrackItem::setColor(int valueRgb)
      {
      if (_trackType == TrackType::PART) {
            _part->setColor(valueRgb);

            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        chan->setColor(valueRgb);
                        }
                  }
            }
      else {
            _chan->setColor(valueRgb);
            }
      }

//---------------------------------------------------------
//   setMute
//---------------------------------------------------------

void MixerTrackItem::setMute(bool value)
      {
      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        if (value)
                              muxseq_stop_notes(chan->channel());
                        chan->setMute(value);
                        }
                  }
            }
      else {
            if (value)
                  muxseq_stop_notes(_chan->channel());
            _chan->setMute(value);
            }
      }

//---------------------------------------------------------
//   setSolo
//---------------------------------------------------------

void MixerTrackItem::setSolo(bool value)
      {
      if (_trackType == TrackType::PART) {
            const InstrumentList* il = _part->instruments();
            for (auto it = il->begin(); it != il->end(); ++it) {
                  Instrument* instr = it->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* chan = playbackChannel(instrChan);
                        chan->setSolo(value);
                        }
                  }
            }
      else {
            _chan->setSolo(value);
            }

      //Go through all channels so that all not being soloed are mute
      int numSolo = 0;
      for (Part* p : _part->score()->parts()) {
            const InstrumentList* il = p->instruments();
            for (auto i = il->begin(); i != il->end(); ++i) {
                  const Instrument* instr = i->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* a = playbackChannel(instrChan);
                        if (a->solo()) {
                              numSolo++;
                              }
                        }
                  }
            }

      for (Part* p : _part->score()->parts()) {
            const InstrumentList* il = p->instruments();
            for (auto i = il->begin(); i != il->end(); ++i) {
                  const Instrument* instr = i->second;
                  for (const Channel* instrChan: instr->channel()) {
                        Channel* a = playbackChannel(instrChan);
                        if (numSolo == 0) {
                              a->setSoloMute(false);
                              }
                        else {
                              a->setSoloMute(!a->solo());
                              if (a->soloMute()) {
                                    muxseq_stop_notes(a->channel());
                                    }
                              }
                        }
                  }
            }
      }

}

