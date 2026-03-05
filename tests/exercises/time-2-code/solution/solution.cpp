#include <iostream>
#include <fstream>
#include <string>

std::string load(const std::string& filename) {
    std::ifstream file(filename);
    std::string user = "";
    if (file.is_open()) {
        std::getline(file, user);
        file.close();
    }
    return user;
}

void save(const std::string& user, const std::string& filename) {
    std::ofstream file(filename);
    if (file.is_open()) {
        file << user << "\n";
        file.close();
    }
}

int main() {
    std::string user = load("datafile.txt");

    if (user == "") {
        std::cout << "Hello, I don't believe we have met.\n";
        std::getline(std::cin, user);
        save(user, "datafile.txt");
        std::cout << "Nice to meet you " << user << ".\n";
    } else {
        std::cout << "It's good to see you again, " << user << ".\n";
    }

    return 0;
}
