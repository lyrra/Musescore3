//=============================================================================
//  MuseSynth
//  Music Software Synthesizer
//
//  Copyright (C) 2013 Werner Schweer
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2
//  as published by the Free Software Foundation and appearing in
//  the file LICENCE.GPL
//=============================================================================

#ifndef __ZONE_H__
#define __ZONE_H__

class Sample;
class Channel;

//---------------------------------------------------------
//   Trigger
//---------------------------------------------------------

enum class Trigger : char {
      ATTACK, RELEASE, FIRST, LEGATO, CC
      };

//---------------------------------------------------------
//   LoopMode
//---------------------------------------------------------

enum class LoopMode : char {
      NO_LOOP, ONE_SHOT, CONTINUOUS, SUSTAIN
      };

//---------------------------------------------------------
//   OffMode
//---------------------------------------------------------

enum class OffMode : char {
      FAST, NORMAL
      };

//---------------------------------------------------------
//   Zone
//---------------------------------------------------------

struct Zone {
      Sample* sample = 0;
      int  offset  = 0;
      int  seq     = 0;
      int seqLen   = 0;
      int seqPos   = 0;

      char keyLo   = 0;
      char keyHi   = 127;
      char veloLo  = 0;
      char veloHi  = 127;
      char keyBase = 60;
      int tune     = 0;       // fine tuning in cent (-100 - +100)
      double pitchKeytrack = 100.0;
      float volume = 1.0;
      float lVol   = .5;
      float rVol   = .5;
      float ampVeltrack  = 100;      // amplifier velocity tracking
      float ampegDelay   = 0.0;
      float ampegStart   = 0.0;
      float ampegAttack  = 1.0;
      float ampegHold    = 0.0;
      float ampegDecay   = 0.0;
      float ampegSustain = 1.0;
      float ampegRelease = 200.0;     // release time in ms
      float ampegVel2Delay    = 0.0;
      float ampegVel2Attack   = 0.0;
      float ampegVel2Hold     = 0.0;
      float ampegVel2Decay    = 0.0;
      float ampegVel2Sustain  = 0.0;
      float ampegVel2Release  = 0.0;
      float rtDecay = 0.0;

      Trigger trigger = Trigger::ATTACK;
      LoopMode loopMode = LoopMode::NO_LOOP;
      OffMode offMode = OffMode::FAST;
      int group = 0;
      int offBy = 0;
      int loopStart, loopEnd;
      double loRand = 0.0;
      double hiRand = 1.0;

      int onLocc[128];
      int onHicc[128];
      int locc[128];
      int hicc[128];
      bool useCC = false;

      Zone();
      ~Zone();
      bool match(Channel*, int key, int velo, Trigger, double rand);
      };

#endif

