import math
import itertools
from functools import reduce

def repair(expenses):

    """
    >>> repair([1721, 979, 979, 366, 299, 675, 1456])
    241861950
    """

    for expenses in itertools.combinations(expenses, 3):
        if sum(expenses) == 2020:
            # return math.prod(expenses) # Python 3.8
            return reduce(lambda x, y: x * y, expenses)

if __name__ == '__main__':
    import doctest
    doctest.testmod()

    data = [int(line) for line in open('adventofcode.input.txt')]
    print(repair(data))
