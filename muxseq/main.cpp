/* GPL-2.0-or-later
 * Copyright (C) 2022 Larry Valkama
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. 
 */

#include "musescore-qt.h"
#include "config.h"
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <zmq.h>
#include <thread>
#include <chrono>
#include <cstring>
#include "mux.h"
#include "muxcommon.h"

namespace Ms {

#define MUX_SYNC_MSLEEP 100

extern bool g_state_play;
bool g_send_heartbeat = false;

void muxseq_threads_start();

std::chrono::high_resolution_clock::time_point g_now;

} // end of namespace Ms

void g_logstr_func (char *str) {
    qDebug(str);
}

int main(int argc, char **argv)
{
    fprintf(stderr, "muxseq initializing\n"); std::fflush(stderr);
    Ms::g_now = std::chrono::high_resolution_clock::now();
    Ms::g_logstr = g_logstr_func;
    Ms::muxseq_threads_start();
    while(1){
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        Ms::g_send_heartbeat = true;
    }
    return 0;
}
