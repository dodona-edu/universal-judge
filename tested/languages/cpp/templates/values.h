#ifndef WRITER_VALUES_H
#define WRITER_VALUES_H

#include <iostream>
#include <vector>
#include <string>
#include <cstdio>

#include "evaluation_result.h"

// Function to escape special characters in a string
std::string escape(const std::string &buffer);

// Function to write a formatted string to an output stream
void write_formatted(FILE* out, const char* format, ...);

// Function to write boolean values
void write_bool(FILE* out, bool value);

// Function to write char values
void write_char(FILE* out, char value);

// Function to write signed char values
void write_signed_char(FILE* out, signed char value);

// Function to write unsigned char values
void write_unsigned_char(FILE* out, unsigned char value);

// Function to write short int values
void write_sint(FILE* out, short int value);

// Function to write unsigned short int values
void write_usint(FILE* out, unsigned short int value);

// Function to write int values
void write_int(FILE* out, int value);

// Function to write unsigned int values
void write_uint(FILE* out, unsigned int value);

// Function to write long values
void write_long(FILE* out, long value);

// Function to write unsigned long values
void write_ulong(FILE* out, unsigned long value);

// Function to write long long values
void write_llong(FILE* out, long long value);

// Function to write unsigned long long values
void write_ullong(FILE* out, unsigned long long value);

// Function to write float values
void write_float(FILE* out, float value);

// Function to write double values
void write_double(FILE* out, double value);

// Function to write long double values
void write_ldouble(FILE* out, long double value);

// Function to write string values
void write_string(FILE* out, const std::string &value);

// Function to write unknown values
void write_unknown(FILE* out, void* value);

// Function to write void values
void write_void(FILE* out, void* value);

// Function to write evaluated results
void write_evaluated(FILE* out, EvaluationResult* result);
#endif //WRITER_VALUES_H
