#include <iostream>
#include <stdarg.h>

#include "values.h"

using namespace std;

// Function to escape special characters in a string
string escape(const string &buffer) {
    string dest;
    dest.reserve(buffer.length() * 2); // Reserve enough space to avoid reallocations
    const string esc_char = "\a\b\f\n\r\t\v\\\"";
    const string essc_str = "abfnrtv\\\"";

    for (char ch : buffer) {
        auto pos = esc_char.find(ch);
        if (pos != string::npos) {
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
    string concatMessages;

    for (const auto& message : result->messages) {
        string messageStr;
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

    string resultStr = result->result ? "true" : "false";
    write_formatted(out, "{"
                        "\"result\": %s, "
                        "\"readable_expected\": \"%s\", "
                        "\"readable_actual\": \"%s\", "
                        "\"messages\": [%s]"
                        "}", resultStr.c_str(), escape(result->readableExpected).c_str(), escape(result->readableActual).c_str(), concatMessages.c_str());

    delete result;
}

// writes an exception to json as
// { "type" : "exception", "message" : "message", "stacktrace" : "stacktrace" }
void write_exception(FILE* out, std::exception_ptr e)
{
    // Stacktrace and exception messages is not easily available in C++
    string json = "{ \"type\" : \"exception\", \"message\" : \"\", \"stacktrace\" : \"\" }";
    fprintf(out, "%s", json.c_str());
}

void write_value(FILE* out)
{
    string json = "{ \"type\" : \"nothing\", \"data\" : null }";
    fprintf(out, "%s", json.c_str());
}
