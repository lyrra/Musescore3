/* GPL-2.0-or-later
 * Copyright (C) 2023 Larry Valkama
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
 */

#include <functional>
#include <atomic>
#include <chrono>
#include <thread>
#include "muxtaskv.h"

namespace Ms {

MuxTaskv::MuxTaskv() {
    m_lock.store(false); // set tasks-vector to being non-accessed
    m_running = true;
}

std::chrono::milliseconds MuxTaskv::scan () {
    auto now = std::chrono::steady_clock::now();
    bool needSleep = false;
    std::chrono::milliseconds soonest(-1);
    for (size_t i = 0; i < m_tasks.size(); i++) {
        auto taskPtr = m_tasks[i];
        if (taskPtr) {
            struct MuxTask* task = static_cast<struct MuxTask*>(taskPtr);
            auto runAfter = task->runAfter;
            // Either run the task if times up, or check how much to sleep
            if (now > runAfter) { // time-up!
                auto func = task->func;
                func();
                /* FIX-20230806: if func() thread dies, we need some monitoring thread that schedules a ping */
                /* FIX-20230806: run the function outside lockdown */
                m_tasks[i] = nullptr;
                // if the tasks has run for some time update 'now'
                now = std::chrono::steady_clock::now();
            } else if (!needSleep || soonest > runAfter - now) {
                soonest = std::chrono::duration_cast<std::chrono::milliseconds>(runAfter - now);
            }
        }
    }
    return soonest;
}

void MuxTaskv::scan_tasks_or_sleep () {
    if (m_tasks.size() == 0) return;
    // acquire lock to tasks-vector
    bool aq = false;
    if (! m_lock.compare_exchange_strong(aq, true)) {
        return; // failed to acquire lock, no big deal, just sleep little
    }
    // tasks-vector lock has been aquired
    std::chrono::milliseconds soonest = scan();
    if (soonest.count() < 0) { // no tasks in future
        m_lock = false;
        return;
    }
    if (soonest.count() > 0) { // tasks in future
        std::this_thread::sleep_for(soonest);
    }
    // release task-vector lock
    m_lock = false;
}


void MuxTaskv::schedule (int after, void (*func)())
{
    bool aq = false;
    while(1) {
        if (m_lock.compare_exchange_strong(aq, true)) {
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    // tasks-vector lock is acquired
    struct MuxTask* task = new MuxTask();
    auto currentTime = std::chrono::steady_clock::now();
    std::chrono::milliseconds durationToAdd(after);
    task->runAfter = currentTime + durationToAdd;
    task->func = func;
    //FIX-20230806: add at first null slot, if exist; if not, then push_back
    m_tasks.push_back(task);
    // release tasks-vector lock
    m_lock = false;
}

bool MuxTaskv::run () {
    scan_tasks_or_sleep();
    return m_running;
}

void MuxTaskv::stop () {
    m_running = false;
}

}
