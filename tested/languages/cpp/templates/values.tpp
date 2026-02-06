#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <array>
#include <list>
#include <tuple>
#include <variant>
#include <any>
#include <sstream>
#include <concepts>
#include <type_traits>
#include <cmath>

// Define a concept to check if a type supports the << operator with std::ostream
template <typename T>
concept Streamable = requires(std::ostream& os, T value) {
    { os << value } -> std::convertible_to<std::ostream&>;
};

template <typename T, typename... U>
concept IsAnyOf = (std::same_as<T, U> || ...);

template<typename T>
std::string to_json(const T& value);
std::string any_to_json_value(const std::any& value);

template<typename T>
std::string getTypeName(const T&) {
    if constexpr (std::is_same_v<T, std::int8_t>) return "int8";
    if constexpr (std::is_same_v<T, std::uint8_t>) return "uint8";
    if constexpr (std::is_same_v<T, std::int16_t>) return "int16";
    if constexpr (std::is_same_v<T, std::uint16_t>) return "uint16";
    if constexpr (std::is_same_v<T, std::int32_t>) return "int32";
    if constexpr (std::is_same_v<T, std::uint32_t>) return "uint32";
    if constexpr (std::is_same_v<T, std::int64_t>) return "int64";
    if constexpr (std::is_same_v<T, std::uint64_t>) return "uint64";
    if constexpr (std::is_same_v<T, int>) return "integer";
    if constexpr (std::is_same_v<T, double>) return "double_precision";
    if constexpr (std::is_same_v<T, float>) return "single_precision";
    if constexpr (std::is_same_v<T, char>) return "char";
    if constexpr (std::is_same_v<T, std::string>) return "text";
    if constexpr (std::is_same_v<T, bool>) return "boolean";
    if constexpr (std::is_same_v<T, std::nullptr_t>) return "null";
    if constexpr (std::is_same_v<T, long double>) return "double_extended";
    if constexpr (std::is_same_v<T, void>) return "nothing";
    return "unknown";
}

// Specializations for container types
template<typename T> std::string getTypeName(const std::vector<T>&) { return "array"; }
template<typename T> std::string getTypeName(const std::set<T>&) { return "set"; }
template<typename T> std::string getTypeName(const std::list<T>&) { return "list"; }
template<typename K, typename V> std::string getTypeName(const std::map<K, V>&) { return "dictionary"; }
template<typename T, std::size_t N> std::string getTypeName(const std::array<T, N>&) { return "array"; }
template<typename... Args> std::string getTypeName(const std::tuple<Args...>&) { return "tuple"; }

template<typename T>
std::string to_json_value(const T& value) {
    if constexpr (std::is_same_v<T, std::any>) {
        return any_to_json_value(value);
    } else if constexpr (std::is_same_v<T, std::string>) {
        return "\"" + escape(value) + "\"";
    } else if constexpr (std::is_same_v<T, char>) {
        if ((unsigned char) value > 127) {
            unsigned char upper = ((unsigned char) value) / 16;
            unsigned char lower = ((unsigned char) value) % 16;
            return "\"0x" + std::string(1, upper < 10 ? upper + '0' : upper - 10 + 'A')  + std::string(1, lower < 10 ? upper + '0' : lower - 10 + 'A') + "\"";
        } else {
            return "\"" + std::string(1, value) + "\"";
        }
    } else if constexpr (std::is_same_v<T, bool>) {
        return value ? "true" : "false";
    } else if constexpr (std::is_same_v<T, std::nullptr_t>) {
        return "null";
    } else if constexpr (std::is_same_v<T, const char*>) {
        return "\"" + escape(std::string(value)) + "\"";
    } else if constexpr (IsAnyOf<T, float, double, long double>) {
        if(std::isnan(value)) {
            return "\"nan\"";
        } else if (std::isinf(value)) {
            return value > 0 ? "\"inf\"" : "\"-inf\"";
        }
        std::ostringstream oss;
        oss << value;
        return oss.str();
    } else if constexpr (IsAnyOf<T, int, std::int8_t, std::uint8_t, std::int16_t, std::uint16_t,
                                         std::int32_t, std::uint32_t, std::int64_t, std::uint64_t,
                                         long, long long, unsigned, unsigned long, unsigned long long>) {
        return std::to_string(value);
    } else if constexpr (Streamable<T>) {
        std::ostringstream oss;
        oss << value;
        return "\"" + escape(oss.str()) + "\"";
    } else {
        return "null"; // default case, we have no info to convert this value.
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

template<typename T, std::size_t N>
std::string to_json_value(const std::array<T, N>& arr) {
    return sequence_to_json_value(arr);
}

template<typename K, typename V>
std::string to_json_value(const std::map<K, V>& map) {
    std::ostringstream result;
    result << "[";
    bool first = true;
    for (const auto& item : map) {
        if (!first) {
            result << ", ";
        }
        result << R"({"key": )" << to_json(item.first);
        result << R"(, "value": )" << to_json(item.second);
        result << " }";
        first = false;
    }
    result << "]";
    return result.str();
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
    json << "{ \"type\" : \"" << type;
    json << "\", \"data\" : " << to_json_value(value);
    if (type == "undefined") {
        std::string diagnostic = typeid(value).name();
        json << ", \"diagnostic\" : \"" << diagnostic << "\"";
    }
    json << " }";
    return json.str();
}

template <typename T> void write_value(std::ostream& out, const T& value)
{
    out << to_json(value);
}
