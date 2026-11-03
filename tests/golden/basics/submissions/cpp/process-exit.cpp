#include <cstdlib>
#include <iostream>
#include <string>

int add(int a, int b) {
    return a + b;
}

std::string greet(std::string name) {
    std::exit(1);
}

void print_greeting(std::string name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}
