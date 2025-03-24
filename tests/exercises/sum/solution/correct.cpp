#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {
    int som = 0;

    for (int i = 1; i < argc; i++) {
        int r = std::atoi(argv[i]);
        if (r == 0 && argv[i][0] != '0') {
            std::cerr << "som: ongeldige argumenten" << std::endl;
            return 1;
        } else {
            som += r;
        }
    }

    std::cout << som << std::endl;

    return 0;
}