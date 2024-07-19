#include <iostream>
#include <vector>
#include <string>
#include <cstdio>
#include <cstring>
#include <cstdarg>
#include <cassert>
#include <cmath>

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

#define FORMAT(name, x) "{\"type\": \"" name "\", \"data\":" x "}"

void write_formatted(FILE* out, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(out, format, args);
    va_end(args);
}

void write_value(FILE* out, bool value) {
    write_formatted(out, FORMAT("boolean", "%s"), value ? "true" : "false");
}

void write_value(FILE* out, char value) {
    string buffer(1, value);
    string result = escape(buffer);
    write_formatted(out, FORMAT("char", "\"%s\""), result.c_str());
}

void write_value(FILE* out, signed char value) {
    write_formatted(out, FORMAT("int8", "%d"), static_cast<int>(value));
}

void write_value(FILE* out, unsigned char value) {
    write_formatted(out, FORMAT("uint8", "%u"), static_cast<unsigned int>(value));
}

void write_value(FILE* out, short int value) {
    write_formatted(out, FORMAT("int16", "%d"), value);
}

void write_value(FILE* out, unsigned short int value) {
    write_formatted(out, FORMAT("uint16", "%u"), value);
}

void write_value(FILE* out, int value) {
    write_formatted(out, FORMAT("int32", "%d"), value);
}

void write_value(FILE* out, unsigned int value) {
    write_formatted(out, FORMAT("uint32", "%u"), value);
}

void write_value(FILE* out, long value) {
    write_formatted(out, FORMAT("int64", "%ld"), value);
}

void write_value(FILE* out, unsigned long value) {
    write_formatted(out, FORMAT("uint64", "%lu"), value);
}

void write_value(FILE* out, long long value) {
    write_formatted(out, FORMAT("integer", "%lld"), value);
}

void write_value(FILE* out, unsigned long long value) {
    write_formatted(out, FORMAT("bigint", "%llu"), value);
}

void write_value(FILE* out, float value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("single_precision", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("single_precision", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("single_precision", "%f"), value);
    }
}

void write_value(FILE* out, double value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("double_precision", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("double_precision", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("double_precision", "%lf"), value);
    }
}

void write_value(FILE* out, long double value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("double_extended", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("double_extended", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("double_extended", "%Lf"), value);
    }
}

void write_value(FILE* out, const string &value) {
    string result = escape(value);
    write_formatted(out, FORMAT("text", "\"%s\""), result.c_str());
}

void write_value(FILE* out, void* value) {
    write_formatted(out, FORMAT("nothing", "\"%s\""), "null");
}

template <typename T> void write_value(FILE* out, T value)
{
    write_formatted(out, FORMAT("unknown", "%p"), "?");
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
