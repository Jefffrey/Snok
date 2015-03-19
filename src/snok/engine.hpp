#pragma once

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>

#include <cmath>

namespace snok {

template<typename IsGame>
sf::RenderWindow&& make_window(IsGame& game) {
    sf::RenderWindow window(sf::VideoMode(800, 600), "Snok");
    window.setVerticalSyncEnabled(true);
    window.setFramerateLimit(game.frame_rate);
    return std::move(window);
}

template<typename IsGame>
void handle_events(sf::RenderWindow window, IsGame& game) {
    sf::Event event;
    while (window.pollEvent(event)) {
        if (event.type == sf::Event::Closed)
            window.close();
        else react(event, game);
    }
}

template<typename IsGame>
sf::Time handle_updates(sf::Time delta, IsGame& game) {
    float update_rate = 1 / game.update_rate; // seconds between updates
    float delta_secs = delta.asSeconds(); // seconds passed
    int num_updates = delta_secs / update_rate; // number of updates to perform
    float remainder = std::fmod(delta_secs, update_rate);
    for (int i = 0; i < num_updates; i++)
        update(game);
    return sf::seconds(remainder);
}

template<typename IsGame>
sf::Time handle_drawing(sf::RenderWindow& window, IsGame& game) {
    draw(window, game);
}

template<typename IsGame>
void run(IsGame& game) {
    auto window = make_window(game);
    auto frame_rate = game.frame_rate;
    sf::Clock delta_clock;
    while (window.isOpen()) {
        sf::Time delta = delta_clock.restart();
        handle_events(window, game);
        handle_updates(delta, game);
        handle_drawing(window, game);
    }
}

} // namespace snok
