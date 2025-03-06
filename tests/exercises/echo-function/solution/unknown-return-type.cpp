#include <string>

class Coord {
public:
    int x;
    int y;

    Coord(const int& x, const int& y) : x(x), y(y) {}
    friend std::ostream& operator<<(std::ostream& os, const Coord& c)
    {
        return os << "Coord[x=" << c.x << ", y=" << c.y << "]";
    }

};

// Function that returns a Coord object
Coord echo(std::string content) {
    return Coord(5, 7);
}
