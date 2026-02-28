#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::string input;
    std::getline(std::cin, input);

    std::ifstream f("input2.txt");
    std::string file_txt;
    if (f && std::getline(f, file_txt) && !file_txt.empty()) {
        std::cout << file_txt << std::endl;
    } else {
        std::cout << input << std::endl;
    }

    return 0;
}
