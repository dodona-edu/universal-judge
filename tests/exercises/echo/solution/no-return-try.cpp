#include <iostream>
#include <string>

int main() try {
    std::string input;
    std::getline(std::cin, input);
    throw input;
} catch (const std::string& input) {
    std::cout << input << "\n";
}
