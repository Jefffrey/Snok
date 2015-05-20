#pragma once

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <snok/types.hpp>

namespace snok {

    class view {
    public: 

        explicit view(size);
        void run();

    private:

        sf::RenderWindow window;

    };

}
