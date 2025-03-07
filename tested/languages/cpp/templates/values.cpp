#include <iostream>
#include <stdarg.h>

#include "values.h"

// Function to escape special characters in a string
std::string escape(const std::string &buffer) {
    std::string dest;
    dest.reserve(buffer.length() * 2); // Reserve enough space to avoid reallocations
    const std::string esc_char = "\a\b\f\n\r\t\v\\\"";
    const std::string essc_str = "abfnrtv\\\"";

    for (char ch : buffer) {
        auto pos = esc_char.find(ch);
        if (pos != std::string::npos) {
            dest += '\\';
            dest += essc_str[pos];
        } else {
            dest += ch;
        }
    }
    return dest;
}

void write_formatted(FILE* out, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(out, format, args);
    va_end(args);
}

// Function to write evaluated results
void write_evaluated(FILE* out, const EvaluationResult& result) {
    std::string concatMessages;

    for (const auto& message : result.messages) {
        std::string messageStr;
        if (message.permission.empty()) {
            messageStr = "{\"description\": \"" + escape(message.description) + "\", \"format\": \"" + escape(message.format) + "\"}";
        } else {
            messageStr = "{\"description\": \"" + escape(message.description) + "\", \"format\": \"" + escape(message.format) + "\", \"permission\": \"" + escape(message.permission) + "\"}";
        }
        if (!concatMessages.empty()) {
            concatMessages += ",";
        }
        concatMessages += messageStr;
    }

    std::string resultStr = result.result ? "true" : "false";
    write_formatted(out, "{"
                        "\"result\": %s, "
                        "\"readable_expected\": \"%s\", "
                        "\"readable_actual\": \"%s\", "
                        "\"messages\": [%s]"
                        "}", resultStr.c_str(), escape(result.readableExpected).c_str(), escape(result.readableActual).c_str(), concatMessages.c_str());
}

std::string exception_message(const std::exception_ptr &eptr = std::current_exception())
{
    if (!eptr) { return ""; }

    try { std::rethrow_exception(eptr); }
    catch (const std::exception &e) { return e.what(); }
    catch (const std::string    &e) { return e; }
    catch (const char           *e) { return e; }
    catch (...)                     { return ""; }
}

std::string exception_type(const std::exception_ptr &eptr = std::current_exception())
{
    if (!eptr) { return ""; }

    try { std::rethrow_exception(eptr); }
    catch (const std::exception &e) { return typeid(e).name(); }
    catch (const std::string    &e) { return "std::string"; }
    catch (const char           *e) { return "char*"; }
    catch (...)                     { return ""; }
}

// writes an exception to json as
// { "type" : "exception", "message" : "message", "stacktrace" : "stacktrace" }
void write_exception(FILE* out, const std::exception_ptr &eptr) {
    // Stacktrace is not easily available in C++
    std::string json = "{ \"type\" : \"%s\", \"message\" : \"%s\", \"stacktrace\" : \"\" }";
    // Whats returned as name is compiler implementation specific
    write_formatted(out, json.c_str(), exception_type(eptr).c_str(), exception_message(eptr).c_str());
}

std::string any_to_json_value(const std::any& value) {
    if (value.type() == typeid(std::string)) {
        return to_json_value(std::any_cast<std::string>(value));
    } else if (value.type() == typeid(char)) {
        return to_json_value(std::any_cast<char>(value));
    } else if (value.type() == typeid(bool)) {
        return to_json_value(std::any_cast<bool>(value));
    } else if (value.type() == typeid(std::nullptr_t)) {
        return to_json_value(std::any_cast<std::nullptr_t>(value));
    } else if (value.type() == typeid(const char*)) {
        return to_json_value(std::any_cast<const char*>(value));
    } else if (value.type() == typeid(float)) {
        return to_json_value(std::any_cast<float>(value));
    } else if (value.type() == typeid(double)) {
        return to_json_value(std::any_cast<double>(value));
    } else if (value.type() == typeid(long double)) {
        return to_json_value(std::any_cast<long double>(value));
    } else if (value.type() == typeid(int)) {
        return to_json_value(std::any_cast<int>(value));
    } else if (value.type() == typeid(long)) {
        return to_json_value(std::any_cast<long>(value));
    } else if (value.type() == typeid(long long)) {
        return to_json_value(std::any_cast<long long>(value));
    } else if (value.type() == typeid(unsigned int)) {
        return to_json_value(std::any_cast<unsigned int>(value));
    } else if (value.type() == typeid(unsigned long)) {
        return to_json_value(std::any_cast<unsigned long>(value));
    } else if (value.type() == typeid(unsigned long long)) {
        return to_json_value(std::any_cast<unsigned long long>(value));
    }
    // We only support any conversion to most basic types
    // as c++ doesn't support casting to generic `vector<x>`
    // supporting more types would explode these if statements

    return "null"; // Default case if type is not supported
}
