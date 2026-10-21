#include <iostream>
#include <stdexcept>
#include <string>

int add(int a, int b) {
    return a + b;
}

std::string greet(std::string name) {
    return "Hello " + name + "!";
}

void print_greeting(std::string name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}
