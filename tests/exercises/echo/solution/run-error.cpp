
#include <iostream>

using namespace std;

int main() {
    int x = 1;
    int y = 0;
    throw runtime_error("Error");

    string input;

    // Read a line from standard input
    getline(cin, input);

    // Output the same line
    cout << input << endl;

    return 0;
}