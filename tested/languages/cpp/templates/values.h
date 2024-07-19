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

template <typename T> void write_value(FILE* out, T value);
void write_value(FILE* out, bool value);
void write_value(FILE* out, char value);
void write_value(FILE* out, signed char value);
void write_value(FILE* out, unsigned char value);
void write_value(FILE* out, short int value);
void write_value(FILE* out, unsigned short int value);
void write_value(FILE* out, int value);
void write_value(FILE* out, unsigned int value);
void write_value(FILE* out, long value);
void write_value(FILE* out, unsigned long value);
void write_value(FILE* out, long long value);
void write_value(FILE* out, unsigned long long value);
void write_value(FILE* out, float value);
void write_value(FILE* out, double value);
void write_value(FILE* out, long double value);
void write_value(FILE* out, const std::string &value);
void write_value(FILE* out, void* value);
void write_value(FILE* out, void* value);

// Function to write evaluated results
void write_evaluated(FILE* out, EvaluationResult* result);
#endif //WRITER_VALUES_H
