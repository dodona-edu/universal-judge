#include <iostream>
#include <vector>
#include <algorithm>

template<typename T, typename S>
std::vector<T> remove(std::vector<T>& l, S v) {
    auto it = std::find(l.begin(), l.end(), v);
    if (it != l.end()) {
        l.erase(it);
    }
    return l;
}
