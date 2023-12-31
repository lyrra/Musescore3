//=============================================================================
//  MuseScore
//  Linux Music Score Editor
//
//  Copyright (C) 2002-2009 Werner Schweer and others
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

#ifndef __DRIVER_H__
#define __DRIVER_H__

namespace Ms {

class NPlayEvent;
enum class Transport : char;


//---------------------------------------------------------
//   Driver
//---------------------------------------------------------

class Driver {

   public:
      Driver() {}
      virtual ~Driver() {}
      virtual bool init(bool hot = false) = 0;
      virtual bool start(bool hotPlug = false) = 0;
      virtual bool stop() = 0;
      virtual void stopTransport() = 0;
      virtual void startTransport() = 0;
      virtual Transport getState() = 0;
      virtual void seekTransport(int) {}
      virtual int sampleRate() const = 0;
      virtual void putEvent(const NPlayEvent&, unsigned /*framePos*/, int portIdx, int channel) {}
      virtual void midiRead() {}
      virtual void handleTimeSigTempoChanged() {}
      virtual int bufferSize() {return 0;}
      virtual void updateOutPortCount(int) {}
      };

extern bool alsaIsUsed, jackIsUsed, portAudioIsUsed, pulseAudioIsUsed;

Driver* driverFactory(std::string driverName);

} // namespace Ms
#endif

