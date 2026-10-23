#include <iostream>
#include <string>

int main(const int argc, char** argv) {
    if (argc < 1) {
        return 1;
    }
    std::string input;
    std::getline(std::cin, input);
    std::cout << input << "\n";
}
