#include <iostream>
#include <stdexcept>
#include <string>

int add(int a, int b) {
    return a + b;
}

std::string greet(std::string name) {
    throw std::runtime_error("cannot greet " + name);
}

void print_greeting(std::string name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}
