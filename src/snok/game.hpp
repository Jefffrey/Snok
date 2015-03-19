#pragma once

namespace snok {

struct game {
    int update_rate; // updates per second
    int frame_rate; // frames per second
    
    game()
        : update_rate(120)
        , frame_rate(60)
        {}

};

} // namespace snok
