#include <thread>
#include <chrono>
#include "mux-qt.h"
#include "event.h"
#include "mux.h"
#include "muxlib.h"

namespace Ms {

void (*g_logstr) (char *str);
thread_local char* threadname = nullptr;
thread_local char _logbuf[256];

void _set_threadname(const char *name) {
    threadname = strdup(name);
}

int overlap_strcat(char* dst, char* src) {
    int i = 0, a = 0, f = 0;
    while (1) {
        if (! f) {
            if (dst[i] == 0) {
                f = 1; // found null-terminator
            }
        }
        if (f) {
            dst[i] = src[a++];
            if (src[a] == 0) {
                dst[++i] = 0;
                i++;
                break;
            }
        }
        i++;
    }
    return i;
}

void get_timestamp (char *buf) {
    struct timespec tp;
    buf[0] = 0;
    uint64_t d = 0;
    if (! clock_gettime(CLOCK_MONOTONIC, &tp)) {
        d = (tp.tv_sec & 0xfff) * 1000000;
        d += tp.tv_nsec / 1000;
    }
    sprintf(buf, "%li ", d);
}

void _log_write () {
    _logbuf[255] = 0; // ambiguous snprintf standard definition
    char t[64+256];
    get_timestamp(t);
    size_t tid = std::hash<std::thread::id>()(std::this_thread::get_id());
    if (threadname) {
        overlap_strcat(t, threadname);
    } else {
        int tn;
        tn = (tid >> 32) ^ (tid && 0xffffffff); // 64bit -> 32bit
        tn = (tn  >> 16) ^ (tn  && 0xffff);     // 32bit -> 16bit
        char ts[6];
        ts[0] = (tn >> 12 & 0xf) + 65;
        ts[1] = (tn >>  8 & 0xf) + 65;
        ts[2] = (tn >>  4 & 0xf) + 65;
        ts[3] = (tn       & 0xf) + 65;
        ts[4] = ' ';
        ts[5] = 0;
        overlap_strcat(t, ts);
    }
    int n = overlap_strcat(t, _logbuf);
    if (n > 0) {
        (*g_logstr)(t);
    }
//    fflush(stdout);
//    int fd = fileno(stdout);
//#ifdef WIN32
//  _commit(fd);
//#else
//  fsync(fd);
//#endif
}
FILE *g_logfp = NULL;

void _logstr (char *str) {
    qDebug(str);
    if (! g_logfp) g_logfp = fopen("ms.log", "w");
    if (g_logfp) {
        const char *nl = "\n";
        fwrite(str, strlen(str), 1, g_logfp);
        fwrite(nl, 1, 1, g_logfp);
        fflush(g_logfp);
    }
}


uint64_t mux_timestamp (std::chrono::high_resolution_clock::time_point start) {
    auto end = std::chrono::high_resolution_clock::now();
    return std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    //std::chrono::duration<double,std::milli> diff = end - start;
    //return diff.count();
}

const char* muxseq_msg_type_info (MuxseqMsgType type) {
    switch (type) {
    case MsgTypeNoop: return "MsgTypeNoop";
    case MsgTypeSeqCreate: return "MsgTypeSeqCreate";
    case MsgTypeSeqInit: return "MsgTypeSeqInit";
    case MsgTypeSeqDeinit: return "MsgTypeSeqDeinit";
    case MsgTypeSeqExit: return "MsgTypeSeqExit";
    case MsgTypeSeqAlive: return "MsgTypeSeqAlive";
    case MsgTypeSeqStart: return "MsgTypeSeqStart";
    case MsgTypeSeqStop: return "MsgTypeSeqStop";
    case MsgTypeSeqSendEvent: return "MsgTypeSeqSendEvent";
    case MsgTypeSeqStartNote: return "MsgTypeSeqStartNote";
    case MsgTypeSeqStartNoteDur: return "MsgTypeSeqStartNoteDur";
    case MsgTypeSeqStopNotes: return "MsgTypeSeqStopNotes";
    case MsgTypeSeqStartNoteTimer: return "MsgTypeSeqStartNoteTimer";
    case MsgTypeSeqStopNoteTimer: return "MsgTypeSeqStopNoteTimer";
    case MsgTypeSeqStopWait: return "MsgTypeSeqStopWait";
    case MsgTypeSeqCurTempo: return "MsgTypeSeqCurTempo";
    case MsgTypeSeqSetRelTempo: return "MsgTypeSeqSetRelTempo";
    case MsgTypeSeqPlaying: return "MsgTypeSeqPlaying";
    case MsgTypeSeqRunning: return "MsgTypeSeqRunning";
    case MsgTypeSeqStarted: return "MsgTypeSeqStarted";
    case MsgTypeSeqStopped: return "MsgTypeSeqStopped";
    case MsgTypeSeqCanStart: return "MsgTypeSeqCanStart";
    case MsgTypeSeqCurTick: return "MsgTypeSeqCurTick";
    case MsgTypeSeqUTick: return "MsgTypeSeqUTick";
    case MsgTypeSeqSeek: return "MsgTypeSeqSeek";
    case MsgTypeSeekEnd: return "MsgTypeSeekEnd";
    case MsgTypeNextMeasure: return "MsgTypeNextMeasure";
    case MsgTypePrevMeasure: return "MsgTypePrevMeasure";
    case MsgTypeNextChord: return "MsgTypeNextChord";
    case MsgTypePrevChord: return "MsgTypePrevChord";
    case MsgTypeRewindStart: return "MsgTypeRewindStart";
    case MsgTypeSetLoopIn: return "MsgTypeSetLoopIn";
    case MsgTypeSetLoopOut: return "MsgTypeSetLoopOut";
    case MsgTypeSetLoopSelection: return "MsgTypeSetLoopSelection";
    case MsgTypeRecomputeMaxMidiOutPort: return "MsgTypeRecomputeMaxMidiOutPort";
    case MsgTypeSeqPreferencesChanged: return "MsgTypeSeqPreferencesChanged";
    case MsgTypeSeqUpdateOutPortCount: return "MsgTypeSeqUpdateOutPortCount";
    case MsgTypeSeqRenderEvents: return "MsgTypeSeqRenderEvents";
    case MsgTypeMasterSynthesizerInit: return "MsgTypeMasterSynthesizerInit";
    case MsgTypeMasterSynthInitInstruments: return "MsgTypeMasterSynthInitInstruments";
    case MsgTypeEOF: return "MsgTypeEOF";
    default: return "ERROR:UNKNOWN-MUXSEQ-MSG-TYPE";
    }
}

const char* muxaudio_msg_type_info(MuxaudioMsgType type) {
    switch (type) {
        case MsgTypeHeartbeat: return "MsgTypeHeartbeat";
        case MsgTypeAudioInit: return "MsgTypeAudioInit";
        case MsgTypeAudioStart: return "MsgTypeAudioStart";
        case MsgTypeAudioStop: return "MsgTypeAudioStop";
        case MsgTypeAudioRunning: return "MsgTypeAudioRunning";
        case MsgTypeJackTransportPosition: return "MsgTypeJackTransportPosition";
        case MsgTypeTransportStart: return "MsgTypeTransportStart";
        case MsgTypeTransportStop: return "MsgTypeTransportStop";
        case MsgTypeTransportSeek: return "MsgTypeTransportSeek";
        case MsgTypeEventToMidi: return "MsgTypeEventToMidi";
        case MsgTypeEventToGui: return "MsgTypeEventToGui";
        case MsgTypeTimeSigTempoChanged: return "MsgTypeTimeSigTempoChanged";
        case MsgTypeOutPortCount: return "MsgTypeOutPortCount";
        default: return "ERROR:UNKNOWN-MUXAUDIO-MSG-TYPE";
    }
}

void muxseq_msg_set_NPlayEvent (MuxseqMsg *msg, NPlayEvent event) {
    msg->payload.sparseEvent.type    = event.type();
    msg->payload.sparseEvent.channel = event.channel();
    msg->payload.sparseEvent.pitch   = event.pitch();
    msg->payload.sparseEvent.velo    = event.velo();
}


} // namespace Ms
