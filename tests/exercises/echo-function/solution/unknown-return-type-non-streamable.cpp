#include <string>

class Coord {
public:
    int x;
    int y;

    Coord(const int& x, const int& y) : x(x), y(y) {}
};

// Function that returns a Coord object
Coord echo(std::string content) {
    return Coord(5, 7);
}
