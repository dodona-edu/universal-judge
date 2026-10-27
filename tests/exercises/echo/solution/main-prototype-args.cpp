#include <iostream>
#include <string>

int main(int argc, char** argv);

int main(int argc, char** argv) {
    std::string input;
    std::getline(std::cin, input);
    std::cout << input << "\n";
}
