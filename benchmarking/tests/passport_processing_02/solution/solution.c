#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define KEYS_LEN 7
const char* const keys[] = { "byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:" };

bool is_valid_field(const char *key, const char *value)
{
    if (strncmp(key, "byr", 3) == 0) {
        int v = atoi(value);
        return v >= 1920 && v <= 2002;
    }

    if (strncmp(key, "iyr", 3) == 0) {
        int v = atoi(value);
        return v >= 2010 && v <= 2020;
    }

    if (strncmp(key, "eyr", 3) == 0) {
        int v = atoi(value);
        return v >= 2020 && v <= 2030;
    }

    if (strncmp(key, "hgt", 3) == 0) {
        int v = atoi(value);
        if (strncmp(value + 3, "cm", 2) == 0)
        {
            return v >= 150 && v <= 193;
        }

        if (strncmp(value + 2, "in", 2) == 0)
        {
            return v >= 59 && v <= 76;
        }

        return false;
    }

    if (strncmp(key, "hcl", 3) == 0) {
        if (value[0] != '#') {
            return false;
        }

        for (int i = 1; i < 7; i++) {
            if (!(value[i] >= 48 && value[i] <= 57) && !(value[i] >= 97 && value[i] <= 102)) {
                return false;
            }
        }

        return true;
    }

    if (strncmp(key, "ecl", 3) == 0) {
        return strncmp(value, "amb", 3) == 0
            || strncmp(value, "blu", 3) == 0
            || strncmp(value, "brn", 3) == 0
            || strncmp(value, "gry", 3) == 0
            || strncmp(value, "grn", 3) == 0
            || strncmp(value, "hzl", 3) == 0
            || strncmp(value, "oth", 3) == 0;
    }

    if (strncmp(key, "pid", 3) == 0) {
        for (int i = 0; i < 9; i++) {
            if (!(value[i] >= 48 && value[i] <= 57)) {
                return false;
            }
        }

        return !(value[9] >= 48 && value[9] <= 57);
    }

    return false;
}

bool is_valid_passport(const char *pass)
{
    for (int i = 0; i < KEYS_LEN; i++) {
        const char *p = strstr(pass, keys[i]);
        if (!p) {
            return false;
        }

        if (!is_valid_field(p, p + 4)) {
            return false;
        }
    }

    return true;
}

int count_valid_passports(const char *filename)
{
    FILE *file = fopen(filename, "r");

    // Include space for newline and string terminator
    char buffer[128] = { 0 };

    uint8_t values = 0;
    int correct = 0;
    while (fgets(buffer, 128, file)) {
        if (buffer[0] == '\n') {
            if (values == 0x7f) {
                correct++;
            }

            values = 0;
        }

        for (int i = 0; i < KEYS_LEN; i++) {
            const char *p = strstr(buffer, keys[i]);
            if (!p) {
                continue;
            }

            if (is_valid_field(p, p + 4)) {
                values = values | (0x1 << i);
            }
        }
    }

    if (values == 0x7f) {
        correct++;
    }

    fclose(file);

    return correct;
}
