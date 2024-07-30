#include <iostream>
#include <string>
#include <vector>
#include <any>
#include <cctype>

bool is_isbn10_st(const std::string &code) {
    // Helper function for computing ISBN-10 check digit
    auto check_digit = [](const std::string &code) -> char {
        int check = 0;
        for (size_t i = 0; i < 9; ++i) {
            check += (i + 1) * (code[i] - '0');
        }
        check %= 11;
        return (check == 10) ? 'X' : (check + '0');
    };

    // Check whether given code contains 10 characters
    if (code.length() != 10) return false;

    // Check whether first nine characters of given code are digits
    for (size_t i = 0; i < 9; ++i) {
        if (!isdigit(code[i])) return false;
    }

    // Check the check digit
    return check_digit(code) == code[9];
}

template <typename T>
bool is_isbn10(const T &code) {
    if(code.type() != typeid(std::string)) return false;
    
    return is_isbn10_st(std::any_cast<std::string>(code));
}

bool is_isbn13_st(const std::string &code) {
    // Helper function for computing ISBN-13 check digit
    auto check_digit = [](const std::string &code) -> char {
        int check = 0;
        for (size_t i = 0; i < 12; ++i) {
            check += ((i % 2 == 0) ? 1 : 3) * (code[i] - '0');
        }
        check = (10 - (check % 10)) % 10;
        return check + '0';
    };

    // Check whether given code contains 13 characters
    if (code.length() != 13) return false;

    // Check whether first twelve characters of given code are digits
    for (size_t i = 0; i < 12; ++i) {
        if (!isdigit(code[i])) return false;
    }

    // Check the check digit
    return check_digit(code) == code[12];
}

template <typename T>
bool is_isbn13(const T &code) {
    if(code.type() != typeid(std::string)) return false;
    
    return is_isbn13_st(std::any_cast<std::string>(code));
}

template <typename T>
bool is_isbn(const T &code, bool isbn13 = true) {
    return isbn13 ? is_isbn13(code) : is_isbn10(code);
}


template <typename T>
std::vector<bool> are_isbn(const std::vector<T> &codes) {
    std::vector<bool> checks;
    for (const auto &code : codes) {
        if(code.type() != typeid(std::string)){
            checks.push_back(false);
            continue;
        }
        std::string isbn = std::any_cast<std::string>(code);
        bool isbn13 = isbn.length() == 13;

        checks.push_back(is_isbn(code, isbn13));
    }
    return checks;
}

template <typename T>
std::vector<bool> are_isbn(const std::vector<T> &codes, bool isbn13) {
    std::vector<bool> checks;
    for (const auto &code : codes) {
        checks.push_back(is_isbn(code, isbn13));
    }
    return checks;
}
