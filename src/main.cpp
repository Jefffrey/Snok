#include <iostream>
#include "snok/engine.hpp"
#include "snok/game.hpp"

auto main() -> int {
    snok::game game;
    snok::run(game);
}
