CFLAGS = -std=c++11 -O2 -Wall -pedantic -pthread
LFLAGS = -framework sfml-window -framework sfml-graphics -framework sfml-system

snok:
	clang++ $(CFLAGS) $(LFLAGS) src/main.cpp -o bin/snok

run:
	./bin/snok
