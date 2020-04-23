//
// Created by strij on 6/03/2020.
//

#ifndef WRITER_VALUES_H
#define WRITER_VALUES_H

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

void write_bool(FILE*, bool);

void write_char(FILE*, char);
void write_uchar(FILE*, unsigned char);
void write_schar(FILE*, signed char);

void write_sint(FILE*, short int);
void write_usint(FILE*, unsigned short int);
void write_int(FILE*, int);
void write_uint(FILE*, unsigned int);
void write_long(FILE*, long int);
void write_ulong(FILE*, unsigned long int);
void write_llong(FILE*, long long int);
void write_ullong(FILE*, unsigned long long int);

void write_float(FILE*, float);
void write_double(FILE*, double);
void write_ldouble(FILE*, long double);

void write_string(FILE*, const char*);

void write_unknown(FILE*, void*);

#define write_value_function(x) _Generic((x),   \
        _Bool: write_bool,                      \
                                                \
        /* Number types */                      \
        char: write_char,                       \
        unsigned char: write_uchar,             \
        signed char: write_schar,               \
        short int: write_sint,                  \
        unsigned short int: write_usint,        \
        int: write_int,                         \
        unsigned int: write_uint,               \
        long int: write_long,                   \
        unsigned long int: write_ulong,         \
        long long int: write_llong,             \
        unsigned long long int: write_ullong,   \
                                                \
        /* Rational stuff */                    \
        float: write_float,                     \
        double: write_double,                   \
        long double: write_ldouble,             \
                                                \
        char *: write_string,                   \
                                                \
                                                \
        default: write_unknown                  \
        )

#define write_value(f, x) write_value_function(x)(f, x)

void send_evaluated(FILE* out, bool result, char* expected, char* actual, size_t nrOfMessages, char** messages);

typedef struct EvaluationResult {
    bool result;
    char* readableExpected;
    char* readableActual;
    char** messages;
    size_t nrOfMessages;
} EvaluationResult;

#endif //WRITER_VALUES_H
