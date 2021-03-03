#include <stdio.h>

#define COLUMNS 8
#define ROWS 128
int seat_id(const char *seat)
{
    int row = 0;
    int row_lower = 0;
    int row_upper = ROWS - 1;
    int column = 0;
    int column_lower = 0;
    int column_upper = COLUMNS - 1;
    int i = 0;
    for (; i < 6; i++) {
        switch (seat[i]) {
            case 'F':
                row_upper -= (row_upper - row_lower + 1) / 2;
                break;
            case 'B':
                row_lower += (row_upper - row_lower + 1) / 2;
                break;
        }
    }

    switch (seat[i++]) {
        case 'F':
            row = row_lower;
            break;
        case 'B':
            row = row_upper;
            break;
    }

    for (; i < 9; i++) {
        switch (seat[i]) {
            case 'L':
                column_upper -= (column_upper - column_lower + 1) / 2;
                break;
            case 'R':
                column_lower += (column_upper - column_lower + 1) / 2;
                break;
        }
    }

    switch (seat[i++]) {
        case 'L':
            column = column_lower;
            break;
        case 'R':
            column = column_upper;
            break;
    }

    return row * COLUMNS + column;
}

int highest_seat_id(const char *filename)
{
    FILE *file = fopen(filename, "r");

    // Include space for newline and string terminator
    char buffer[16] = { 0 };

    int max = 0;

    while (fgets(buffer, 16, file)) {
        int tmp = seat_id(buffer);
        if (tmp > max) {
            max = tmp;
        }
    }

    return max;
}
int row(const char *seat)
{
    int end_res = 0;
    for (int i = 0; i < 7; i++) {
        if (seat[i] == 'B') {
            end_res |= 0x40 >> i;
        }
    }

    return end_res;
}

int column(const char *seat)
{
    int end_res = 0;
    for (int i = 7; i < 10; i++) {
        if (seat[i] == 'R') {
            end_res |= 0x200 >> i;
        }
    }

    return end_res;
}
