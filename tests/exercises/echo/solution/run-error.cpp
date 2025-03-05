#include <iostream>
#include <exception>

int main() {
    int x = 1;
    int y = 0;
    throw std::runtime_error("Error");

    std::string input;

    // Read a line from standard input
    std::getline(std::cin, input);

    // Output the same line
    std::cout << input << std::endl;

    return 0;
}
