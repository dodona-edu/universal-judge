#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INC_RIGHT 3

int count_trees(const char * filename)
{
    FILE * file = fopen(filename, "r");

    // Include space for newline and string terminator
    char buffer[64] = { 0 };
    // Start is always safe
    fgets(buffer, 64, file);
    // strlen includes the newline
    int width = strchr(buffer, '\n') - buffer;

    int pos = INC_RIGHT;
    int hit = 0;
    while (fgets(buffer, 64, file)) {
        if (buffer[pos] == '#') {
            hit++;
        }

        pos = (pos + INC_RIGHT) % width;
    }

    return hit;
}
