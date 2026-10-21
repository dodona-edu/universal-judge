#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    std::cerr << "debug: " << argc - 1 << " arguments" << std::endl;

    if (argc < 2) {
        return 1;
    }

    std::string prefix = argv[1];
    for (int i = 2; i < argc; i++) {
        std::cout << prefix << ": " << argv[i] << std::endl;
    }

    std::string line;
    while (std::getline(std::cin, line)) {
        std::cout << prefix << ": " << line << std::endl;
    }
    return 0;
}
