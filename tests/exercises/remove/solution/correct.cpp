#include <iostream>
#include <vector>

template<typename T, typename S>
std::vector<T> remove(const std::vector<T>& l, const S& value) {
    std::vector<T> result;
    for (const T& x : l) {
        if (x != value) {
            result.push_back(x);
        }
    }
    return result;
}
