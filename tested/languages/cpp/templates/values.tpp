#include <vector>
#include <string>
#include <cstdio>
#include <cstring>
#include <cstdarg>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <set>
#include <map>
#include <list>
#include <tuple>
#include <iostream>
#include <string>
#include <typeinfo>
#include <type_traits>
#include <vector>
#include <set>
#include <map>
#include <array>
#include <list>
#include <tuple>
#include <any>
#include <sstream>

template<typename T>
string to_json(const T& value);


template<typename T>
string getTypeName(const T&) {
    if (is_same<T, int8_t>::value) return "int8";
    if (is_same<T, uint8_t>::value) return "uint8";
    if (is_same<T, int16_t>::value) return "int16";
    if (is_same<T, uint16_t>::value) return "uint16";
    if (is_same<T, int32_t>::value) return "int32";
    if (is_same<T, uint32_t>::value) return "uint32";
    if (is_same<T, int64_t>::value) return "int64";
    if (is_same<T, uint64_t>::value) return "uint64";
    if (is_same<T, int>::value) return "integer";
    if (is_same<T, double>::value) return "double_precision";
    if (is_same<T, float>::value) return "single_precision";
    if (is_same<T, char>::value) return "char";
    if (is_same<T, string>::value) return "text";
    if (is_same<T, bool>::value) return "boolean";
    if (is_same<T, nullptr_t>::value) return "null";
    if (is_same<T, long double>::value) return "double_extended";
    if (is_same<T, void>::value) return "nothing";
    return "undefined";
}

// Specialization for vector
template<typename T>
string getTypeName(const vector<T>&) {
    return "array";
}

// Specialization for set
template<typename T>
string getTypeName(const set<T>&) {
    return "set";
}

// Specialization for map
template<typename K, typename V>
string getTypeName(const map<K, V>&) {
    return "dictionary";
}

// Specialization for array
template<typename T, size_t N>
string getTypeName(const array<T, N>&) {
    return "array";
}

// Specialization for list
template<typename T>
string getTypeName(const list<T>&) {
    return "list";
}

// Specialization for tuple
template<typename... Args>
string getTypeName(const tuple<Args...>&) {
    return "tuple";
}

template<typename T>
string to_json_value(const T& value) {
    if constexpr (is_same<T, string>::value) {
        return "\"" + escape(value) + "\"";
    } else if constexpr (is_same<T, char>::value) {
        return "\"" + string(1, value) + "\"";
    } else if constexpr (is_same<T, bool>::value) {
        return value ? "true" : "false";
    } else if constexpr (is_same<T, nullptr_t>::value) {
        return "null";
    } else if constexpr (is_same<T, const char*>::value) {
        return "\"" + string(value) + "\"";
    } else if constexpr (is_same<T, float>::value || is_same<T, double>::value || is_same<T, long double>::value) {
        ostringstream oss;
        oss << value;
        return oss.str();
    } else {
        return to_string(value);
    }
}

// helper vector, list, set, ...
template<typename T, template<typename, typename...> typename S>
string sequence_to_json_value(const S<T>& sequence) {
    string result = "[";
    for (const auto& item : sequence) {
        result += to_json(item) + ",";
    }
    if (!sequence.empty()) {
        result.pop_back(); // remove trailing comma
    }
    result += "]";
    return result;
}

template<typename T>
string to_json_value(const vector<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename T>
string to_json_value(const list<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename T>
string to_json_value(const set<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename K, typename V>
string to_json_value(const map<K, V>& map) {
    string result = "[";
    for (const auto& item : map) {
        result += "{";
        result += "\"key\": " + to_json(item.first) + ", ";
        result += "\"value\": " + to_json(item.second);
        result += "},";
    }
    if (!map.empty()) {
        result.pop_back(); // remove trailing comma
    }
    result += "]";
    return result;
}

template<typename T, size_t N>
string to_json_value(const array<T, N>& arr) {
    string result = "[";
    for (const auto& item : arr) {
        result += to_json(item) + ",";
    }
    if (N > 0) {
        result.pop_back(); // remove trailing comma
    }
    result += "]";
    return result;
}

template<typename... Args>
string to_json_value(const tuple<Args...>& tup);

template<size_t Index = 0, typename... Args>
typename enable_if<Index == sizeof...(Args), void>::type
to_json_value_helper(const tuple<Args...>& tup, string& result) {}

template<size_t Index = 0, typename... Args>
typename enable_if<Index < sizeof...(Args), void>::type
to_json_value_helper(const tuple<Args...>& tup, string& result) {
    result += to_json(get<Index>(tup)) + ",";
    to_json_value_helper<Index + 1>(tup, result);
}

template<typename... Args>
string to_json_value(const tuple<Args...>& tup) {
    string result = "[";
    to_json_value_helper(tup, result);
    if (sizeof...(Args) > 0) {
        result.pop_back(); // remove trailing comma
    }
    result += "]";
    return result;
}

template<typename T>
string to_json(const T& value) {
    string json = "{ \"type\" : \"" + getTypeName(value) + "\", \"data\" : " + to_json_value(value) + " }";
    return json;
}

template <typename T> void write_value(FILE* out, const T& value)
{
    string json = to_json(value);
    fprintf(out, "%s", json.c_str());
}
