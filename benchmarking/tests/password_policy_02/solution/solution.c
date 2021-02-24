#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int lower;
    int upper;
    char letter;
} pattern_tuple;

void parse_pattern(char* pattern, pattern_tuple* tuple);

bool is_valid_password(char* password, char* pattern) {
    pattern_tuple* tuple = (pattern_tuple*)malloc(sizeof(pattern_tuple));
    tuple->lower = 0;
    tuple->upper = 0;
    parse_pattern(pattern, tuple);
    int lower = tuple->lower -1;
    int upper = tuple->upper -1;
    char letter = tuple->letter;
    free(tuple);
    return (password[lower] == letter && password[upper] != letter) || (password[lower] != letter && password[upper] == letter);
}

void parse_pattern(char* pattern, pattern_tuple* tuple) {
    int i = 0;
    while (pattern[i] != '-') {
        tuple->lower = (tuple->lower *10) + (pattern[i] - '0');
        i++;
    }
    i++;
    while (pattern[i] != ' ') {
        tuple->upper = (tuple->upper *10) + (pattern[i] - '0');
        i++;
    }
    tuple->letter = pattern[++i];
}
