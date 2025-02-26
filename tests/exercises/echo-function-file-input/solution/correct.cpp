#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cctype>

std::string trim(const std::string& str) {
    std::string trimmed = str;
    trimmed.erase(trimmed.begin(), std::find_if(trimmed.begin(), trimmed.end(), [](int ch) {
        return !std::isspace(ch);
    }));
    trimmed.erase(std::find_if(trimmed.rbegin(), trimmed.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), trimmed.end());
    return trimmed;
}

std::string echo_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        return "Error: Failed to open file.";
    }

    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    return trim(content);
}
