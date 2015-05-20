#include <snok/view.hpp>

namespace snok {

    view::view(size s)
        : window(sf::VideoMode(s[0], s[1]), "Snok") {
        window.setFramerateLimit(60);
    }

    void view::run() {
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

}
