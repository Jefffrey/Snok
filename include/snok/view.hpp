#pragma once

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <snok/types.hpp>

namespace snok {

    class view {
    public: 

        view(size s)
            : window(sf::VideoMode(s[0], s[1]), "Snok") {
            window.setFramerateLimit(60);
        }

        void run() {
            while (window.isOpen()) {
               sf::Event event;
               while (window.pollEvent(event)) {
                   if (event.type == sf::Event::Closed)
                       window.close();
               }
               window.clear();
               window.display();
            }
        }

    private:

        sf::RenderWindow window;

    };

}
