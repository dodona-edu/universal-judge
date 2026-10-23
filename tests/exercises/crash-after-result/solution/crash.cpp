#include <string>

int add(int a, int b) {
    return a + b;
}

std::string greet(std::string name) {
    volatile char* nowhere = nullptr;
    nowhere[0] = name[0];
    return "Hello, " + name + "!";
}
