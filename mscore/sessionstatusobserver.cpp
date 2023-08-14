//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2020 MuseScore BVBA
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

#include "musescore-qt.h"
#include "musescore-gui-qt.h"
#include "sessionstatusobserver.h"
#include "musescore.h"

namespace Ms {

void SessionStatusObserver::prevSessionStatus(bool sessionFileFound, const QString& sessionFullVersion, bool clean)
      {
      Q_UNUSED(sessionFileFound);
      Q_UNUSED(sessionFullVersion);
      Q_UNUSED(clean);
      }

} // namespace Ms
