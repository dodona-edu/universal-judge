#include <stdio.h>
#include <stdlib.h>
int row(char * input)
{
    int i = 0;
    int res = 0;
    for (i = 0; i < 7; i++)
    {
        res *= 2;
        if (input[i] == 'B')
        {
            res++;
        }
    }
    return res;
}

int column(char * input)
{
    int i = 0;
    int res = 0;
    for (i = 7; i < 10; i++)
    {
        res *= 2;
        if (input[i] == 'R')
        {
            res++;
        }
    }
    return res;
}

int seat_id(char * input)
{
    return (row(input) * 8 + column(input));
}

int cmpfunc (const void * a, const void * b) {
   return ( *(int*)a - *(int*)b );
}

int missing_seat_id(char * file)
{
    char buff[1000];
    int seats[1024];
    int numSeats = 0;
    int i = 0;
    FILE * input = fopen(file, "r");
    while(fgets(buff,sizeof(buff), input))
    {
        int val = seat_id(buff);
        seats[numSeats] = val;
        numSeats++;
    }

    qsort(seats, numSeats, sizeof(int), cmpfunc);

    for (i = 1; i < numSeats; i++)
    {
        if (seats[i] - seats[i-1] == 2)
        {
            return seats[i] - 1;
        }
    }

    return -1;

}
