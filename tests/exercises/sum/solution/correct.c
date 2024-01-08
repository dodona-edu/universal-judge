#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

// Taken from https://stackoverflow.com/questions/7021725/how-to-convert-a-string-to-integer-in-c
int str2int(long *out, char *s, int base) {
    char *end;
    if (s[0] == '\0' || isspace(s[0]))
        return -1;
    errno = 0;
    long l = strtol(s, &end, 10);
    /* Both checks are needed because INT_MAX == LONG_MAX is possible. */
    if (l > INT_MAX || (errno == ERANGE && l == LONG_MAX))
        return -1;
    if (l < INT_MIN || (errno == ERANGE && l == LONG_MIN))
        return -1;
    if (*end != '\0')
        return -1;
    *out = l;
    return 0;
}

int main(int argc, char** argv) {

    long sum = 0;

    for (int i = 1; i < argc; i++) {
        char* arg = argv[i];
        long number;
        int result = str2int(&number, arg, 10);
        if (result == 0) {
            sum += number;
        } else {
            fprintf(stderr, "som: ongeldige argumenten\n");
            exit(1);
        }
    }

    printf("%ld\n", sum);
    exit(0);
}
