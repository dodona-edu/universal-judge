#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Helper to strip whitespace/newlines
void trim(char *str) {
    int len = strlen(str);
    while (len > 0 && isspace(str[len - 1])) {
        str[--len] = '\0';
    }
}

void load(const char *filename, char *user, int max_len) {
    FILE *file = fopen(filename, "r");
    if (file) {
        if (fgets(user, max_len, file) == NULL) {
            user[0] = '\0';
        }
        fclose(file);
        trim(user);
    } else {
        user[0] = '\0';
    }
}

void save(const char *user, const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file) {
        fprintf(file, "%s\n", user);
        fclose(file);
    }
}

int main() {
    char user[256] = {0};
    load("datafile.txt", user, sizeof(user));

    if (strlen(user) == 0) {
        printf("Hello, I don't believe we have met.\n");
        fgets(user, sizeof(user), stdin);
        trim(user);
        save(user, "datafile.txt");
        printf("Nice to meet you %s.\n", user);
    } else {
        printf("It's good to see you again, %s.\n", user);
    }

    return 0;
}
