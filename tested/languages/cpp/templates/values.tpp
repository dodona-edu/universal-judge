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
#include <variant>
#include <sstream>

template<typename T>
std::string to_json(const T& value);
std::string any_to_json_value(const std::any& value);

template<typename T>
std::string getTypeName(const T&) {
    if (std::is_same<T, std::int8_t>::value) return "int8";
    if (std::is_same<T, std::uint8_t>::value) return "uint8";
    if (std::is_same<T, std::int16_t>::value) return "int16";
    if (std::is_same<T, std::uint16_t>::value) return "uint16";
    if (std::is_same<T, std::int32_t>::value) return "int32";
    if (std::is_same<T, std::uint32_t>::value) return "uint32";
    if (std::is_same<T, std::int64_t>::value) return "int64";
    if (std::is_same<T, std::uint64_t>::value) return "uint64";
    if (std::is_same<T, int>::value) return "integer";
    if (std::is_same<T, double>::value) return "double_precision";
    if (std::is_same<T, float>::value) return "single_precision";
    if (std::is_same<T, char>::value) return "char";
    if (std::is_same<T, std::string>::value) return "text";
    if (std::is_same<T, bool>::value) return "boolean";
    if (std::is_same<T, std::nullptr_t>::value) return "null";
    if (std::is_same<T, long double>::value) return "double_extended";
    if (std::is_same<T, void>::value) return "nothing";
    return "unknown";
}

// Specialization for vector
template<typename T>
std::string getTypeName(const std::vector<T>&) {
    return "array";
}

// Specialization for set
template<typename T>
std::string getTypeName(const std::set<T>&) {
    return "set";
}

// Specialization for map
template<typename K, typename V>
std::string getTypeName(const std::map<K, V>&) {
    return "dictionary";
}

// Specialization for array
template<typename T, size_t N>
std::string getTypeName(const std::array<T, N>&) {
    return "array";
}

// Specialization for list
template<typename T>
std::string getTypeName(const std::list<T>&) {
    return "list";
}

// Specialization for tuple
template<typename... Args>
std::string getTypeName(const std::tuple<Args...>&) {
    return "tuple";
}

template<typename T>
std::string to_json_value(const T& value) {
    if constexpr (std::is_same<T, std::any>::value) {
        return any_to_json_value(value);
    } else if constexpr (std::is_same<T, std::string>::value) {
        return "\"" + escape(value) + "\"";
    } else if constexpr (std::is_same<T, char>::value) {
        return "\"" + std::string(1, value) + "\"";
    } else if constexpr (std::is_same<T, bool>::value) {
        return value ? "true" : "false";
    } else if constexpr (std::is_same<T, std::nullptr_t>::value) {
        return "null";
    } else if constexpr (std::is_same<T, const char*>::value) {
        return "\"" + std::string(value) + "\"";
    } else if constexpr (std::is_same<T, float>::value || std::is_same<T, double>::value || std::is_same<T, long double>::value) {
        if(std::isnan(value)) {
            return "\"nan\"";
        } else if (std::isinf(value) && value > 0) {
            return "\"inf\"";
        } else if (std::isinf(value) && value < 0) {
            return "\"-inf\"";
        }

        std::ostringstream oss;
        oss << value;
        return oss.str();
    } else if constexpr (std::is_same<T, int>::value
                        || std::is_same<T, std::int8_t>::value
                        || std::is_same<T, std::uint8_t>::value
                        || std::is_same<T, std::int16_t>::value
                        || std::is_same<T, std::uint16_t>::value
                        || std::is_same<T, std::int32_t>::value
                        || std::is_same<T, std::uint32_t>::value
                        || std::is_same<T, std::int64_t>::value
                        || std::is_same<T, std::uint64_t>::value
                        || std::is_same<T, long>::value
                        || std::is_same<T, long long>::value
                        || std::is_same<T, unsigned>::value
                        || std::is_same<T, unsigned long>::value
                        || std::is_same<T, unsigned long long>::value) {
        return std::to_string(value);
    } else {
        std::ostringstream oss;
        oss << value;
        return "\"" + escape(oss.str()) + "\"";
    }
}

// helper vector, list, set, ...
template<typename T, template<typename, typename...> typename S>
std::string sequence_to_json_value(const S<T>& sequence) {
    std::string result = "[";
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
std::string to_json_value(const std::vector<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename T>
std::string to_json_value(const std::list<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename T>
std::string to_json_value(const std::set<T>& sequence) {
    return sequence_to_json_value(sequence);
}

template<typename ...Ts>
std::string to_json_value(const std::variant<Ts...>& variant) {
    return std::visit(to_json_value, variant);
}

template<typename K, typename V>
std::string to_json_value(const std::map<K, V>& map) {
    std::string result = "[";
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

template<typename T, std::size_t N>
std::string to_json_value(const std::array<T, N>& arr) {
    std::string result = "[";
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
std::string to_json_value(const std::tuple<Args...>& tup);

template<std::size_t Index = 0, typename... Args>
typename std::enable_if<Index == sizeof...(Args), void>::type
to_json_value_helper(const std::tuple<Args...>& tup, std::string& result) {}

template<std::size_t Index = 0, typename... Args>
typename std::enable_if<Index < sizeof...(Args), void>::type
to_json_value_helper(const std::tuple<Args...>& tup, std::string& result) {
    result += to_json(std::get<Index>(tup)) + ",";
    to_json_value_helper<Index + 1>(tup, result);
}

template<typename... Args>
std::string to_json_value(const std::tuple<Args...>& tup) {
    std::string result = "[";
    to_json_value_helper(tup, result);
    if (sizeof...(Args) > 0) {
        result.pop_back(); // remove trailing comma
    }
    result += "]";
    return result;
}

template<typename T>
std::string to_json(const T& value) {
    std::string type = getTypeName(value);

    std::ostringstream json;
    json << "{ \"type\" : \"" << type << "\", \"data\" : " << to_json_value(value);
    if (type == "undefined"){
        std::string diagnostic = typeid(value).name();
        json << ", \"diagnostic\" : \"" << diagnostic << "\"";
    }
    json <<  " }";
    return json.str();
}

template <typename T> void write_value(FILE* out, const T& value)
{
    std::string json = to_json(value);
    fprintf(out, "%s", json.c_str());
}
