#include <stdio.h>
#include <stdlib.h>

#define TREE '#'

int count_trees(int dx, int dy, char const *path)
{
    FILE *fp = fopen(path, "r");
    int x = 0, y = 0, i = 0, width, trees = 0;
    char *row;
    char c;

    // Get line width
    while ((c = fgetc(fp)) != '\n')
        i++;

    width = i;
    i = 0;
    row = (char *)malloc(width * sizeof(char));

    // Follow path
    while (c != EOF)
    {
        while ((row[i++] = (c = fgetc(fp))) != '\n' && c != EOF)
            ;
        i = 0;
        y++;

        if (c != EOF && y % dy == 0 && row[(x = (x + dx) % width)] == TREE)
            trees++;
    }

    free(row);
    fclose(fp);

    return trees;
}
