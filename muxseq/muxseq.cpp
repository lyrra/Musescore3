/* SPDX-License-Identifier: GPL-3.0-or-later
 * Copyright (C) 2022 Larry Valkama
 */
/*
    mscore: musescore/gui, this is the application where you can see your score
    muxseq: program that runs the sequencer and synthesizers
    muxaudio: program that handles your audio drivers

                +----------------------------------+ 
    mscore      | Muxseq                           |      muxaudio
         |      |                                  |      |
         | ---> |               g_ringBufferStereo | ---> |
         |      |                                  |      |
         | ZMQ  |                                  | ZMQ  |
         |      |                                  |      |
         | <--- |                 g_msg_from_audio | <--- |       
         |      |                                  |      |
                +----------------------------------+     

*/

#include "musescore-qt.h"
#include "config.h"
#include <iostream>
#include <thread>
#include <chrono>
#include <zmq.h>
#include "event.h"
#include "msynthesizer.h"
#include "mux.h"
#include "muxcommon.h"
#include "muxlib.h"
#include "muxseq.h"
#include "muxseq_api.h"
#include "seq.h"
#include "muxqueue.h"

// Avoid sleeping less than 1000 microseconds
#define MUX_READER_MSLEEP 1
#define MUX_WRITER_MSLEEP 1

namespace Ms {

extern bool g_send_heartbeat;
int muxseq_mscore_tell (MuxseqMsgType type, int i);

void mux_send_event_to_gui(struct SparseEvent se);
void mux_audio_send_event_to_midi(struct MuxaudioMsg msg);
bool g_state_play = false;
extern int g_driver_running;
uint64_t g_utick = -1;

static std::vector<std::thread> seqThreads;
int g_muxseq_audio_process_run = 0;


extern struct Mux::MuxSocket g_muxsocket_muxaudioQueryClientCtrl;

extern Seq* g_seq;

int seq_create(int sampleRate); // FIX: move into seq.h

/*
 * message queue, between audio and mux
 */

#define MAILBOX_SIZE 256
struct MuxaudioMsg g_msg_from_audio[MAILBOX_SIZE];
int g_msg_from_audio_reader = 0;
int g_msg_from_audio_writer = 0;

/*
 * message queue, between mscore and muxseq
 */

struct MuxQueue *queue_from_mscore;

// audio-thread uses this function to send messages to mux/mscore
int mux_mq_from_audio_writer_put (struct MuxaudioMsg msg) {
    memcpy(&g_msg_from_audio[g_msg_from_audio_writer].payload, &msg.payload, sizeof(msg.payload));
    // setting the type will signal to the reader that this slot is filled
    g_msg_from_audio[g_msg_from_audio_writer].type = msg.type;
    g_msg_from_audio_writer = (g_msg_from_audio_writer + 1) % MAILBOX_SIZE;
    return 1;
}


int mux_mq_from_audio_reader_visit () {
    if (g_msg_from_audio_reader == g_msg_from_audio_writer) {
        return 0;
    }
    struct MuxaudioMsg msg = g_msg_from_audio[g_msg_from_audio_reader];
    LD2("MUXAUDIO q=> MUXSEQ msg %s", muxaudio_msg_type_info(msg.type));
    int rc = 0; //FIX muxseq_mq_from_muxaudio_handle(msg);
    g_msg_from_audio_reader = (g_msg_from_audio_reader + 1) % MAILBOX_SIZE;
    return rc;
}


int muxseq_mq_from_mscore_handle (struct MuxseqMsg *msg) {
    LD2("muxseq_mq_from_mscore_handle msg %s", muxseq_msg_type_info(msg->type));
    return 0;
}

int muxseq_from_mscore_reader_visit () {
    unsigned char* msg = (unsigned char*) mux_mq_read(queue_from_mscore);
    int type;
    if (! msg) { // no message on queue
        return 0;
    }
    memcpy(&type, msg, 4);
    switch (type) {
        case MsgTypeMasterSynthInitInstruments:
            {
                int maxMidiPorts;
                int numSevs;
                memcpy(&maxMidiPorts, msg+4, 4);
                memcpy(&numSevs, msg+8, 4);
                LD4("MSCORE i=> MUXSEQ msg %s maxMidiPorts=%i numSevs=%i", muxseq_msg_type_info((MuxseqMsgType) type), maxMidiPorts, numSevs);
                struct SparseMidiEvent *sevs = (struct SparseMidiEvent *) (msg + 12);
                if (g_seq) {
                    g_seq->initInstruments(maxMidiPorts, numSevs, sevs);
                }
                free(msg);
                return 1;
            }
        break;
        case MsgTypeSeqStartNoteDur:
            {
                LD("MSCORE i=> MUXSEQ msg %s", muxseq_msg_type_info((MuxseqMsgType) type));
                struct MuxseqMsg *mx = (struct MuxseqMsg*) msg;
                if (g_seq) {
                    g_seq->startNote(mx->payload.sparseEvent);
                }
                free(msg);
                return 1;
            }
        default:
            LD("MSCORE i=> MUXSEQ msg %s UNKNOWN INTERNAL MESSAGE", muxseq_msg_type_info((MuxseqMsgType) type));
    }
    free(msg);
    return 0;
}

/* message to/from audio helpers
 */
void muxseq_msg_to_audio(MuxaudioMsgType typ, int val)
{
    // put message on MQ towards audio thread (second part of mux)
    struct MuxaudioMsg msg;
    msg.type = typ;
    msg.payload.i = val;
    if (mux_zmq_ctrl_send_to_audio(msg) < 0) {
        return;
    }
    if (zmq_recv(g_muxsocket_muxaudioQueryClientCtrl.socket, &msg, sizeof(struct MuxaudioMsg), 0) < 0) {
        LD("muxseq_msg_to_audio failed to recv reply from muxaudio.");
    }
}

void mux_msg_from_audio(MuxaudioMsgType typ, int val)
{
    struct MuxaudioMsg msg;
    msg.type = typ;
    msg.payload.i = val;
    mux_mq_from_audio_writer_put(msg);
}

void msgToAudioSeekTransport(int utick) {
    muxseq_msg_to_audio(MsgTypeTransportSeek, utick);
}

/*
 * Audio ringbuffer from mux to audio
 */
#define MUX_CHAN 2
#define MUX_RINGSIZE  (128*MUX_CHAN)

unsigned int g_ringBufferWriterStart = 0;
unsigned int g_ringBufferReaderStart = 0;
struct MuxaudioBuffer g_ringBufferStereo[MUX_RINGSIZE];

unsigned int g_readerCycle = 0;
unsigned int g_writerCycle = 0;
unsigned int g_readerPause = 0;
unsigned int g_writerPause = 0;

// this function is called by the zmq-network audio connection towards muxaudio
struct MuxaudioBuffer* mux_process_bufferStereo () {
    int slept = 0;
    for (int r = 0;; r++) {
        unsigned int newReaderPos = (g_ringBufferReaderStart + 1) % MUX_RINGSIZE;
        /*
        // FIX: What about: |--R---------r---W--|
        if ((g_ringBufferReaderStart > newReaderPos &&
             g_ringBufferReaderStart < g_ringBufferWriterStart &&
             newReaderPos < g_ringBufferReaderStart)) {
            LE("ERROR");
        }
        */
        // ensure we dont read into writers buffer part
        if (// ringbuffer is empty
            g_ringBufferReaderStart == g_ringBufferWriterStart ||
            // if reader wraps around and goes beyond writer
            // |--W----R--------r--|
            // -------->        ---,
            (g_ringBufferReaderStart > newReaderPos &&
             newReaderPos > g_ringBufferWriterStart) ||
            // FIX: What about: |--R---------r---W--|
            // no wrap, writer is at right side of reader, but new reader goes beyond writer
            // |--r----W----R------|
            //    ---------->
            (g_ringBufferWriterStart > g_ringBufferReaderStart &&
             newReaderPos > g_ringBufferWriterStart)) {
            g_readerPause++;
            slept += MUX_READER_MSLEEP;
            std::this_thread::sleep_for(std::chrono::milliseconds(MUX_READER_MSLEEP));
        } else { // there is enough room in reader-part of ring-buffer to use
            if (r || slept) {
                LD("WARNING had to wait %ims (%i, %i * %i), due to no available audio buffer",
                   r * MUX_READER_MSLEEP ,
                   slept, r, MUX_READER_MSLEEP);
            }
            return g_ringBufferStereo + newReaderPos;
        }
    }
}

void mux_advance_bufferStereo () {
    unsigned int newReaderPos = (g_ringBufferReaderStart + 1) % MUX_RINGSIZE;
    if (newReaderPos < g_ringBufferReaderStart) {
        g_readerCycle++;
    }
    g_ringBufferReaderStart = newReaderPos;
}

int muxseq_audio_process_work () {
    unsigned int newWriterPos = (g_ringBufferWriterStart + 1) % MUX_RINGSIZE;

    // ensure we dont overwrite part of buffer that is being read by reader
    // if writer wraps around and goes beyond reader
    // |--W----R--------w--|  (w,W: old,new writer  R: reader)
    // --->             ---,
    if (g_ringBufferReaderStart < g_ringBufferWriterStart &&
        g_ringBufferWriterStart > newWriterPos &&
        newWriterPos >= g_ringBufferReaderStart) {
        return 0;
    // wraps around, but goes beyond reader
    // |--R----W--------w--|
    // -------->        ---,
    } else if (g_ringBufferWriterStart < g_ringBufferReaderStart &&
               newWriterPos < g_ringBufferWriterStart) {
        return 0;
    // no wrap, reader is at right side of writer, but new writer goes beyond reader
    // |--w----R---W-------|
    //    --------->
    } else if (g_ringBufferWriterStart < g_ringBufferReaderStart &&
               newWriterPos >= g_ringBufferReaderStart) {
        return 0;
    }

    struct MuxaudioBuffer* mabuf;
    // process a chunk of frames, ie fill the chunk with audio content
    mabuf = g_ringBufferStereo + newWriterPos;
    memset(mabuf, 0, sizeof(struct MuxaudioBuffer));
    g_seq->process(mabuf);

    if (newWriterPos < g_ringBufferWriterStart) {
        g_writerCycle++;
    }
    g_ringBufferWriterStart = newWriterPos;
    return 1;
}

void muxseq_muxaudioWorker_process() {
    int slept = MUX_WRITER_MSLEEP;
    while (true) {
        bool workDone = false;
        if (g_muxseq_audio_process_run) {
            if (muxseq_audio_process_work() || // audio work was done,
                mux_mq_from_audio_reader_visit() || // got message from audio via MQ
                muxseq_from_mscore_reader_visit()) { // got message from mscore via MQ
                workDone = true;
            }
        }

        if (g_send_heartbeat) {
            muxseq_msg_to_audio(MsgTypeHeartbeat, 0);
            g_send_heartbeat = false;
            workDone = true;
        }

        if (! workDone) {
            g_writerPause++;
            std::this_thread::sleep_for(std::chrono::milliseconds(slept));
        }
    }
    LD("MUXSEQ audio-process terminated.");
}

int mux_zmq_ctrl_send_to_audio (struct MuxaudioMsg msg)
{
    LD("MUXSEQ ==> MUXAUDIO msg %s", muxaudio_msg_type_info(msg.type));
    if (zmq_send(g_muxsocket_muxaudioQueryClientCtrl.socket, &msg, sizeof(struct MuxaudioMsg), 0) < 0) {
        LD("ERROR sending to MUXAUDIO msg %s", muxaudio_msg_type_info(msg.type));
        return -1;
    }
    return 0;
}

int muxseq_handle_musescore_reply_int (Mux::MuxSocket &sock, struct MuxseqMsg msg, int i) {
    msg.payload.i = i;
    return zmq_send(sock.socket, &msg, sizeof(struct MuxseqMsg), 0);
}
int muxseq_handle_musescore_reply_bool (Mux::MuxSocket &sock, struct MuxseqMsg msg, bool b) {
    msg.payload.b = b;
    return zmq_send(sock.socket, &msg, sizeof(struct MuxseqMsg), 0);
}

int muxseq_handle_musescore_msg_SeqAlive (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    return muxseq_handle_musescore_reply_int(sock, msg, 1); // always alive
}

int muxseq_handle_musescore_msg_SeqRunning (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD("got message from musescore, SeqRunning=%i", g_driver_running);
    return muxseq_handle_musescore_reply_int(sock, msg, g_driver_running);
}

int muxseq_handle_musescore_msg_SeqPreferencesChanged (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    // FIX: copy preferences?
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg_SeqCreate (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    int sampleRate = msg.payload.i;
    LD("MSCORE ==> MUXSEQ msg create Sequencer, sampleRate=%i", sampleRate);
    int rc = seq_create(sampleRate);
    LD("create synthesizer");
    if(muxseq_create_synti(sampleRate) == nullptr) {
        rc = -1;
    }
    return muxseq_handle_musescore_reply_int(sock, msg, rc);
}

int muxseq_handle_musescore_msg_SeqInit (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD("MSCORE ==> MUXSEQ msg init Sequencer, hotPlug=%i", msg.payload.b);
    if (g_seq == nullptr) {
        LE("cant initialize sequencer: it doesnt exist!");
    } else {
        g_seq->init(msg.payload.b); // FIX: use the return value to reply_bool
    }
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg_SeqStopNoteTimer (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg_SeqExit (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    // FIX: destroy seq
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg_MsgTypeSeqCanStart (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    // FIX: query muxaudio if sequencer can start
    return muxseq_handle_musescore_reply_bool(sock, msg, true);
}
int muxseq_handle_musescore_msg_MsgTypeSeqStart (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    int rc = 0;
    if (g_seq) {
        g_seq->start(); // FIX: really call seq from this thread?
    } else {
        LE("cant play sequencer: it doesnt exist!");
        rc = -1;
    }
    return muxseq_handle_musescore_reply_int(sock, msg, rc);
}

int muxseq_handle_musescore_msg_MsgTypeSeqStop (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD4("handle MsgTypeSeqStop");
    int rc = 0;
    // FIX: SEQ doesn't do anything on gui-initiated-playstop ?
    if (g_seq) {
        g_seq->guiStop(); // FIX: really call seq from this thread?
        g_seq->stop(); // FIX: really call seq from this thread?
    }
    return muxseq_handle_musescore_reply_int(sock, msg, rc);
}

int muxseq_handle_musescore_msg_MsgTypeSeqStopWait (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD4("handle MsgTypeSeqStopWait");
    int rc = 0;
    // FIX: SEQ doesn't do anything on gui-initiated-playstop ?
    if (g_seq) {
        g_seq->guiStop(); // FIX: really call seq from this thread?
        g_seq->stop(); // FIX: really call seq from this thread?
    }
    return muxseq_handle_musescore_reply_int(sock, msg, rc);
}

int muxseq_handle_musescore_msg_MsgTypeSeqSeek (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD("MSCORE ==> MUXSEQ msg %s tick=%i", muxseq_msg_type_info(msg.type), msg.payload.i);
    if (g_seq) {
        g_seq->seek(msg.payload.i);
    }
    muxseq_msg_to_audio(MsgTypeTransportSeek, msg.payload.i);
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg_MsgTypeSeqPlaying (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD("MSCORE ==> MUXSEQ msg %s tick=%i", muxseq_msg_type_info(msg.type), msg.payload.i);
    return muxseq_handle_musescore_reply_int(sock, msg, g_state_play ? 1 : 0);
}

int muxseq_handle_musescore_msg_MsgTypeSeqStartNoteDur (Mux::MuxSocket &sock, struct MuxseqMsg msg) {
    LD("MSCORE ==> MUXSEQ msg %s", muxseq_msg_type_info(msg.type));
    int rc = 0;
    int len = sizeof(struct MuxseqMsg);
    void* data = malloc(len);
    memcpy(data, (void *) &msg, len);
    if (mux_mq_write(queue_from_mscore, data) < 0) {
        LE("ERROR MSCORE ==> MUXSEQ msg %s mailbox is full", muxseq_msg_type_info(msg.type));
        rc = -1;
    }
    return muxseq_handle_musescore_reply_int(sock, msg, rc);
}

int muxseq_handle_musescore_msg_MsgTypeMasterSynthInitInstruments(Mux::MuxSocket &sock, void *buf) {
    unsigned char *ptr = (unsigned char*) buf;
    int type;
    int maxMidiPorts;
    int numSevs;
    memcpy(&type, buf, 4);
    memcpy(&maxMidiPorts, ptr + 4, 4);
    memcpy(&numSevs, ptr + 8, 4);
    LD("MSCORE ==> MUXSEQ msg %s (%i) maxMidiPorts=%i numSevs=%i", muxseq_msg_type_info((MuxseqMsgType)type), type, maxMidiPorts, numSevs);
    if (g_seq) {
        int len = sizeof(struct SparseMidiEvent) * numSevs;
        void* data = malloc(12 + len);
        memcpy(data, buf, 12 + len);
        if (mux_mq_write(queue_from_mscore, data) < 0) {
            LE("ERROR MSCORE ==> MUXSEQ msg %s mailbox is full", muxseq_msg_type_info((MuxseqMsgType)type));
        }
    } else {
        LD("cant initInstruments, no sequencer");
    }
    struct MuxseqMsg msg;
    msg.type = (MuxseqMsgType) type;
    return muxseq_handle_musescore_reply_int(sock, msg, 0);
}

int muxseq_handle_musescore_msg(Mux::MuxSocket &sock, void *buf)
{
    struct MuxseqMsg msg = *((struct MuxseqMsg *) buf);
    LD("MSCORE ==> MUXSEQ msg %s", muxseq_msg_type_info(msg.type));
    switch (msg.type) {
        case MsgTypeSeqAlive:
            return muxseq_handle_musescore_msg_SeqAlive(sock, msg);
        case MsgTypeSeqRunning:
            return muxseq_handle_musescore_msg_SeqRunning(sock, msg);
        case MsgTypeSeqPreferencesChanged:
            return muxseq_handle_musescore_msg_SeqPreferencesChanged(sock, msg);
        case MsgTypeSeqCreate:
            return muxseq_handle_musescore_msg_SeqCreate(sock, msg);
        case MsgTypeSeqInit:
            return muxseq_handle_musescore_msg_SeqInit(sock, msg);
        case MsgTypeSeqStopNoteTimer:
            return muxseq_handle_musescore_msg_SeqStopNoteTimer(sock, msg);
        case MsgTypeSeqExit:
            return muxseq_handle_musescore_msg_SeqExit(sock, msg);
        case MsgTypeSeqCanStart:
            return muxseq_handle_musescore_msg_MsgTypeSeqCanStart(sock, msg);
        case MsgTypeSeqStart:
            return muxseq_handle_musescore_msg_MsgTypeSeqStart(sock, msg);
        case MsgTypeSeqStop:
            return muxseq_handle_musescore_msg_MsgTypeSeqStop(sock, msg);
        case MsgTypeSeqSeek:
            return muxseq_handle_musescore_msg_MsgTypeSeqSeek(sock, msg);
        case MsgTypeSeqStopWait:
            return muxseq_handle_musescore_msg_MsgTypeSeqStopWait(sock, msg);
        case MsgTypeSeqStartNoteDur:
            return muxseq_handle_musescore_msg_MsgTypeSeqStartNoteDur(sock, msg);
        case MsgTypeMasterSynthInitInstruments:
            return muxseq_handle_musescore_msg_MsgTypeMasterSynthInitInstruments(sock, buf);
        case MsgTypeSeqPlaying:
            return muxseq_handle_musescore_msg_MsgTypeSeqPlaying(sock, msg);
        // MsgTypeSeqStopWait
        default:
            LD("ERROR: Unknown message: %s", muxseq_msg_type_info(msg.type));
        break;
    }
    return 0;
}

int muxseq_handle_muxaudioQueryClient_msg_AudioBufferFeed (Mux::MuxSocket &sock, struct MuxaudioMsg msg)
{
    struct MuxaudioBuffer* mabuf = mux_process_bufferStereo();
    zmq_send(sock.socket, mabuf, sizeof(struct MuxaudioBuffer), 0);

    uint64_t utick = msg.payload.i; // this is the tick that is being played in real-time
    if (g_utick != utick) {
        g_utick = utick;
        muxseq_mscore_tell(MsgTypeSeqUTick, utick);
    }

    mux_advance_bufferStereo();
    return 0;
}

int muxseq_handle_muxaudioQueryClient_msg (Mux::MuxSocket &sock, struct MuxaudioMsg msg)
{
    switch (msg.type) {
        case MsgTypeAudioBufferFeed:
            return muxseq_handle_muxaudioQueryClient_msg_AudioBufferFeed(sock, msg);
        //FIX: perhaps not all these can be done from this thread?
        case MsgTypeAudioRunning:
            //g_driver_running = msg.payload.i;
            //FIX: upon connection to muxaudio, we need to get the state (which contains g_driver_running) (ie, we are a slow-subscriber in zmq terms)
            LD("---- g_driver_running is running? %i", msg.payload.i);
        break;
        case MsgTypeJackTransportPosition:
            //LD("mux_set_jack_position to %i", msg.payload.jackTransportPosition.frame);
            mux_set_jack_position(msg.payload.jackTransportPosition);
        break;
        case MsgTypeEventToGui:
            mux_send_event_to_gui(msg.payload.sparseEvent);
        break;
        default: // this should not happen
            LD("audio-query-client WARNING: unknown msg from muxaudio: %i", msg.type);
            // skip this message
            // g_msg_from_audio_reader = (g_msg_from_audio_reader + 1) % MAILBOX_SIZE;
    }
    msg.type = MsgTypeAudioNoop;
    zmq_send(sock.socket, &msg, sizeof(struct MuxaudioMsg), 0);
    return 0;
}


/* muxaudioQueryServer -- handle requests from muxaudio
 *
 */
void muxseq_muxaudioQueryClient_mainloop(Mux::MuxSocket &sock)
{
    LD("MUXSEQ <== MUXAUDIO -- query client started");
    g_driver_running = 1; // FIX: this is needed, because we doesnt ask muxaudio about its current state
    while (1) {
        struct MuxaudioMsg msg;
        if (zmq_recv(sock.socket, &msg, sizeof(struct MuxaudioMsg), 0) < 0) {
            LE("audioQueryClient zmq-recv error: %s", strerror(errno));
            break;
        }
        muxseq_handle_muxaudioQueryClient_msg(sock, msg);
    }
    LD("mux_network_mainloop audio has exited");
}

/* mscoreQueryServer -- handle requests from musescore
 *
 */
void muxseq_mscoreQueryServer_mainloop(Mux::MuxSocket &sock) {
    LD("MSCORE ==> MUXSEQ -- query server started");
    while(1){
        zmq_msg_t msg;
        int rc = zmq_msg_init (&msg);
        if (rc != 0) break;
        rc = zmq_msg_recv (&msg, sock.socket, 0);
        if (rc == -1) {
            LE("mscoreQueryServer zmq-recv error: %s", strerror(errno));
            break;
        }
        LD("MSCORE ==> MUXSEQ -- got message %i bytes", rc);
        muxseq_handle_musescore_msg(sock, zmq_msg_data(&msg));
        zmq_msg_close (&msg);
        std::this_thread::sleep_for(std::chrono::milliseconds(MUX_READER_MSLEEP));
    }
}

/* mscoreQueryReqServer -- handle outgoing requests to musescore
 *
 */
void muxseq_mscoreQueryReqServer_mainloop(Mux::MuxSocket &sock) {
    LD("MUXSEQ ==> MSCORE -- queryReq server started");
    while(1){
        // the other threads (who wants to send requests towards musescore)
        // will use the socket directly, but we could instead consider
        // going through an ringbuffer, and this thread would then be the single reader
        std::this_thread::sleep_for(std::chrono::milliseconds(MUX_WRITER_MSLEEP));
    }
}

} // end of namespace MS
