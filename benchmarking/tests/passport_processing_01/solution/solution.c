#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INC_RIGHT 3

bool is_valid_passport(const char *pass)
{
    return strstr(pass, "byr:")
        && strstr(pass, "iyr:")
        && strstr(pass, "eyr:")
        && strstr(pass, "hgt:")
        && strstr(pass, "hcl:")
        && strstr(pass, "ecl:")
        && strstr(pass, "pid:");
}

int count_valid_passports(const char *filename)
{
    FILE *file = fopen(filename, "r");

    // Include space for newline and string terminator
    char buffer[128] = { 0 };

    bool has_byr = false;
    bool has_iyr = false;
    bool has_eyr = false;
    bool has_hgt = false;
    bool has_hcl = false;
    bool has_ecl = false;
    bool has_pid = false;
    int correct = 0;
    while (fgets(buffer, 128, file)) {
        if (buffer[0] == '\n') {
            if (has_byr && has_iyr && has_eyr && has_hgt && has_hcl && has_ecl && has_pid) {
                correct++;
            }

            has_byr = false;
            has_iyr = false;
            has_eyr = false;
            has_hgt = false;
            has_hcl = false;
            has_ecl = false;
            has_pid = false;
        }

        has_byr = has_byr || strstr(buffer, "byr:");
        has_iyr = has_iyr || strstr(buffer, "iyr:");
        has_eyr = has_eyr || strstr(buffer, "eyr:");
        has_hgt = has_hgt || strstr(buffer, "hgt:");
        has_hcl = has_hcl || strstr(buffer, "hcl:");
        has_ecl = has_ecl || strstr(buffer, "ecl:");
        has_pid = has_pid || strstr(buffer, "pid:");
    }

    if (has_byr && has_iyr && has_eyr && has_hgt && has_hcl && has_ecl && has_pid) {
        correct++;
    }

    fclose(file);

    return correct;
}
