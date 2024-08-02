#include <iostream>
#include <set>
#include <random>
#include <algorithm>
#include <sstream>
#include <iterator>

std::string loterij(int aantal = 6, int maximum = 42) {
    std::set<int> getallen;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, maximum);

    while (getallen.size() < aantal) {
        getallen.insert(dis(gen));
    }

    std::ostringstream oss;
    std::copy(getallen.rbegin(), getallen.rend(), std::ostream_iterator<int>(oss, " - "));
    std::string result = oss.str();
    result = result.substr(0, result.length() - 3); // Remove the trailing " - "
    return result;
}
