def count_trees(dc, dr, filename):

    """
    >>> count_trees(1, 1, 'slope.txt')
    2
    >>> count_trees(3, 1, 'slope.txt')
    7
    >>> count_trees(5, 1, 'slope.txt')
    3
    >>> count_trees(7, 1, 'slope.txt')
    4
    >>> count_trees(1, 2, 'slope.txt')
    2
    """

    """
    >>> count_trees(1, 1, 'adventofcode.input.txt')
    64
    >>> count_trees(3, 1, 'adventofcode.input.txt')
    259
    >>> count_trees(5, 1, 'adventofcode.input.txt')
    65
    >>> count_trees(7, 1, 'adventofcode.input.txt')
    59
    >>> count_trees(1, 2, 'adventofcode.input.txt')
    35

    >>> total = 1
    >>> for dc, dr in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]:
    ...     total *= count_trees(dc, dr, 'adventofcode.input.txt')
    >>> total
    2224913600
    """

    # read the slope
    with open(filename, 'r') as configuration:
        slope = [row.rstrip('\n') for row in configuration.readlines()]

    # determine number of rows and cols in the slope
    rows, cols = len(slope), len(slope[0])

    # count the number of trees down the slope
    c, r = 0, 0

    trees = 0
    while r < rows:

        # check if there's a three along the slope
        if slope[r][c % cols] == '#':
            trees += 1

        # go down the slope
        r += dr
        c += dc

    # return the number of trees down the slope
    return trees

if __name__ == '__main__':
    import doctest
    doctest.testmod()
