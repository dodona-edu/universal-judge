#include <string>

int add(int a, int b) {
    return a + b;
}

std::string greet(std::string name) {
    volatile bool forever = true;
    while (forever) {}
    return "Hello, " + name + "!";
}
