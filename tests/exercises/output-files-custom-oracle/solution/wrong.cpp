#include <fstream>
#include <vector>

void generate_even() {
    std::ofstream outfile("even.txt");
    std::vector<int> evens = {9, 8, 6, 4, 2};
    for (int n : evens) {
        outfile << n << "\n";
    }
    outfile.close();
}
