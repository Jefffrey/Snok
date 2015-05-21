#pragma once

#include <chrono>

namespace snok {

    // Stateful timer that triggers a certain action every 
    // specified interval. It keeps track of remainders, so that
    // partial periods are preserved.
    template<typename Interval, typename Trigger = std::function<void()>>
    class timer {
    public:

        using period_type = Interval;
        using trigger_type = Trigger;

        timer(period_type dur)
            : period(dur)
            {}

        void set_trigger(trigger_type trig) {
            trigger = std::move(trig);
        }

        template<typename... Args>
        void advance(period_type delta) {
            remainder += delta;
            while (remainder >= period) {
                trigger();
                remainder -= period;
            }
        }

    private:

        const period_type period;
        trigger_type trigger;
        period_type remainder;

    };

}
