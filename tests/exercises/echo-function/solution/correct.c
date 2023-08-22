#include <stdlib.h>
#include <stdio.h>

char* echo(char* content) {
    return content;
}


void no_echo(char* content) {
    // Do nothing.
}


char* to_string(int number) {
    int size = snprintf(NULL, 0, "%d", number);
    char* the_string = malloc((size + 1) * sizeof(char));
    sprintf(the_string, "%d", number);
    return the_string;
}
