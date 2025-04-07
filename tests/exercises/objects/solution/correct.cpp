#include <iostream>
#include <set>
#include <vector>

class EqualChecker {
public:
    EqualChecker(int number) : number(number) {}

    bool check(int other) {
        return other == number;
    }

    int prop;
private:
    int number;
};

std::set<std::vector<int>> setTest() {
    std::set<std::vector<int>> mySet;
    mySet.insert({1, 2});
    mySet.insert({2, 3});
    return mySet;
}
