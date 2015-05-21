#include <iostream>
#include <vector>
#include <string>

int main(int argc, char const* argv[]) {
    std::vector<std::string> args(argv, argv + argc);
    if (argc != 2) {
        std::cerr << "Usage: ./server <port>\n";
        return 1;
    } else {
        auto port = args[1];
        std::cout << "Server ready, listening to port " << port << '\n';
        return 0;
    }
}
