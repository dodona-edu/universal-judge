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

// Format macro equivalent
#define FORMAT(name, x) "{\"type\": \"" name "\", \"data\":" x "}"

// Function to write a formatted string to an output stream
void write_formatted(FILE* out, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(out, format, args);
    va_end(args);
}

// Function to write boolean values
void write_bool(FILE* out, bool value) {
    write_formatted(out, FORMAT("boolean", "%s"), value ? "true" : "false");
}

// Function to write char values
void write_char(FILE* out, char value) {
    string buffer(1, value);
    string result = escape(buffer);
    write_formatted(out, FORMAT("char", "\"%s\""), result.c_str());
}

// Function to write signed char values
void write_signed_char(FILE* out, signed char value) {
    write_formatted(out, FORMAT("int8", "%d"), static_cast<int>(value));
}

// Function to write unsigned char values
void write_unsigned_char(FILE* out, unsigned char value) {
    write_formatted(out, FORMAT("uint8", "%u"), static_cast<unsigned int>(value));
}

// Function to write short int values
void write_sint(FILE* out, short int value) {
    write_formatted(out, FORMAT("int16", "%d"), value);
}

// Function to write unsigned short int values
void write_usint(FILE* out, unsigned short int value) {
    write_formatted(out, FORMAT("uint16", "%u"), value);
}

// Function to write int values
void write_int(FILE* out, int value) {
    write_formatted(out, FORMAT("int32", "%d"), value);
}

// Function to write unsigned int values
void write_uint(FILE* out, unsigned int value) {
    write_formatted(out, FORMAT("uint32", "%u"), value);
}

// Function to write long values
void write_long(FILE* out, long value) {
    write_formatted(out, FORMAT("int64", "%ld"), value);
}

// Function to write unsigned long values
void write_ulong(FILE* out, unsigned long value) {
    write_formatted(out, FORMAT("uint64", "%lu"), value);
}

// Function to write long long values
void write_llong(FILE* out, long long value) {
    write_formatted(out, FORMAT("integer", "%lld"), value);
}

// Function to write unsigned long long values
void write_ullong(FILE* out, unsigned long long value) {
    write_formatted(out, FORMAT("bigint", "%llu"), value);
}

// Function to write float values
void write_float(FILE* out, float value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("single_precision", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("single_precision", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("single_precision", "%f"), value);
    }
}

// Function to write double values
void write_double(FILE* out, double value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("double_precision", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("double_precision", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("double_precision", "%lf"), value);
    }
}

// Function to write long double values
void write_ldouble(FILE* out, long double value) {
    if (isnan(value)) {
        write_formatted(out, FORMAT("double_extended", "\"%s\""), "nan");
    } else if (isinf(value)) {
        write_formatted(out, FORMAT("double_extended", "\"%s\""), value < 0 ? "-inf" : "inf");
    } else {
        write_formatted(out, FORMAT("double_extended", "%Lf"), value);
    }
}

// Function to write string values
void write_string(FILE* out, const string &value) {
    string result = escape(value);
    write_formatted(out, FORMAT("text", "\"%s\""), result.c_str());
}

// Function to write unknown values
void write_unknown(FILE* out, void* value) {
    write_formatted(out, FORMAT("unknown", "\"%s\""), "?");
}

// Function to write void values
void write_void(FILE* out, void* value) {
    write_formatted(out, FORMAT("nothing", "\"%s\""), "null");
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
