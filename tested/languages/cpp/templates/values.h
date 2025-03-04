#ifndef WRITER_VALUES_H
#define WRITER_VALUES_H

#include <iostream>
#include <vector>
#include <string>
#include <cstdio>
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

#include "evaluation_result.h"

string escape(const string &buffer);

// Function to write a value as a json object with type information to a file
template <typename T> void write_value(FILE* out, const T& value);

// Function to write evaluated results
void write_evaluated(FILE* out, EvaluationResult* result);

// writes an exception to json as
// { "type" : "exception", "message" : "message", "stacktrace" : "stacktrace" }
void write_exception(FILE* out, const std::exception& e);

// Include the implementation file for template functions
#include "values.tpp"

#endif //WRITER_VALUES_H
