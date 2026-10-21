#include <iostream>
#include <stdexcept>
#include <string>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        return 1;
    }

    std::string prefix = argv[1];
    for (int i = 2; i < argc; i++) {
        std::cout << prefix << ": " << argv[i] << std::endl;
        throw std::runtime_error("cannot continue");
    }

    std::string line;
    while (std::getline(std::cin, line)) {
        std::cout << prefix << ": " << line << std::endl;
        throw std::runtime_error("cannot continue");
    }
    return 0;
}
