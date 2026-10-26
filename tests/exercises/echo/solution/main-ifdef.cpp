#include <iostream>
#include <string>

#ifdef X
int main() {
#else
int main(void) {
#endif
    std::string input;
    std::getline(std::cin, input);
    std::cout << input << "\n";
}
