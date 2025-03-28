#ifndef WRITER_VALUES_H
#define WRITER_VALUES_H

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cstdio>
#include <typeinfo>
#include <type_traits>
#include <exception>
#include <set>
#include <map>
#include <array>
#include <list>
#include <tuple>
#include <any>

#include "evaluation_result.h"

std::string escape(const std::string &buffer);

// Function to write a value as a json object with type information to a file
template <typename T> void write_value(std::ostream& out, const T& value);

// Function to write evaluated results
void write_evaluated(std::ostream& out, const EvaluationResult& result);

// writes an exception to json as
// { "type" : "exception", "message" : "message", "stacktrace" : "stacktrace" }
void write_exception(std::ostream& out, const std::exception_ptr &eptr);

// Include the implementation file for template functions
#include "values.tpp"

#endif //WRITER_VALUES_H
