/* GPL-2.0-or-later
 * Copyright (C) 2023 Larry Valkama
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
 */
#include <vector>

namespace Ms {

struct MuxTask {
    std::chrono::time_point<std::chrono::steady_clock> runAfter;
    std::function<void()> func;
};

class MuxTaskv
{
private:

    bool m_running = true;
    std::vector<struct MuxTask *> m_tasks;
    std::atomic<bool> m_lock; // true when queue is being accessed

    std::chrono::milliseconds scan ();
    void scan_tasks_or_sleep ();

public:
    MuxTaskv();
    void schedule (int after, void (*func)());
    bool run ();
    void stop ();
};

}
