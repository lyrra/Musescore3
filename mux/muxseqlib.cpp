
#include "audio/midi/event.h"
//#include "../mscore/seq.h"

namespace Ms {


void muxseq_send_event(NPlayEvent event) {
    //FIX: send the event over muxseq-zmq
    //seq->sendEvent(event);
}

void muxseq_start_note(int channel, int pitch, int velocity, double nt) {
    //FIX: send the event over muxseq-zmq
    //seq->startNote(channel,
    //               pitch,
    //               velocity,
    //               nt);
}

} // namespace Ms
