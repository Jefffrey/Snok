#pragma once

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <snok/types.hpp>

namespace snok {

    // Represents a client view of a given world.
    class view {
    public: 

        explicit view(size);
        void run();

    private:

        sf::RenderWindow window;
        sf::Font font;
        sf::Text text;

    };

}
