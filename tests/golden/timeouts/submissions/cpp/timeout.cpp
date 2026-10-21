#include <iostream>
#include <string>

void print_greeting(std::string name) {
    volatile bool forever = true;
    while (forever) {}
    std::cout << "Hello, " << name << "!" << std::endl;
}
