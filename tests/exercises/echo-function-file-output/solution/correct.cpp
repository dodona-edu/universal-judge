#include <iostream>
#include <fstream>
#include <string>

void echo_function(const std::string& filename, const std::string& content) {
    // Create an output file stream
    std::ofstream outputFile(filename);

    // Check if the file was opened successfully
    if (outputFile.is_open()) {
        // Write the content to the file
        outputFile << content;
        // add newline
        outputFile << "\n";

        // Close the file
        outputFile.close();
    }
}
