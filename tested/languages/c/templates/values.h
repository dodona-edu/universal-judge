//
// Created by strij on 6/03/2020.
//

#ifndef WRITER_VALUES_H
#define WRITER_VALUES_H

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "evaluation_result.h"

void write_bool(FILE*, bool);

void write_char(FILE*, char);

void write_signed_char(FILE*, signed char);
void write_unsigned_char(FILE*, unsigned char);
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
void write_void(FILE*, void*);

#define write_value_function(x) _Generic((x),   \
        _Bool: write_bool,                      \
        void*: write_void,                       \
                                                \
        /* Number types */                      \
        char: write_char,                       \
        unsigned char: write_unsigned_char,     \
        signed char: write_signed_char,         \
        short int: write_sint,                  \
        unsigned short int: write_usint,        \
        int: write_int,                         \
        unsigned int: write_uint,               \
        long int: write_long,                   \
        unsigned long int: write_ulong,         \
        long long int: write_llong,             \
        unsigned long long int: write_ullong,   \
                                                \
        /* Real stuff */                        \
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

void write_evaluated(FILE* out, EvaluationResult* result);

#endif //WRITER_VALUES_H
