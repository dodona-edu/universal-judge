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
void write_evaluated(FILE* out, EvaluationResult* result) {
    std::string concatMessages;

    for (const auto& message : result->messages) {
        std::string messageStr;
        if (message->permission.empty()) {
            messageStr = "{\"description\": \"" + escape(message->description) + "\", \"format\": \"" + escape(message->format) + "\"}";
        } else {
            messageStr = "{\"description\": \"" + escape(message->description) + "\", \"format\": \"" + escape(message->format) + "\", \"permission\": \"" + escape(message->permission) + "\"}";
        }
        if (!concatMessages.empty()) {
            concatMessages += ",";
        }
        concatMessages += messageStr;
    }

    std::string resultStr = result->result ? "true" : "false";
    write_formatted(out, "{"
                        "\"result\": %s, "
                        "\"readable_expected\": \"%s\", "
                        "\"readable_actual\": \"%s\", "
                        "\"messages\": [%s]"
                        "}", resultStr.c_str(), escape(result->readableExpected).c_str(), escape(result->readableActual).c_str(), concatMessages.c_str());

    delete result;
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
