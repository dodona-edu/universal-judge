def count_trees(filename):

    """
    >>> count_trees('slope.txt')
    7
    >>> count_trees('adventofcode.input.txt')
    259
    """

    # read the slope
    with open(filename, 'r') as configuration:
        slope = [row.rstrip('\n') for row in configuration.readlines()]

    # determine number of rows and cols in the slope
    rows, cols = len(slope), len(slope[0])

    # count the number of trees down the slope
    c, r = 0, 0
    dc, dr = 3, 1

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
