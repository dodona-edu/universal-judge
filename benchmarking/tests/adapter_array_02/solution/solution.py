def read_charges(filename):

    with open(filename) as charges:
        return [int(charge) for charge in charges]

def arrangements(filename):

    """
    >>> arrangements('adapters1.txt')
    8
    >>> arrangements('adapters2.txt')
    19208
    >>> arrangements('adventofcode.input.txt')
    3022415986688
    """

    # read the list of charges
    charges = read_charges(filename)

    # add first and last charge
    charges.append(0)
    charges.append(max(charges) + 3)

    # sort the charges
    charges = sorted(charges)

    # initialize the arrangements
    arrangements = [0] * len(charges)

    # compute arrangements in succession (dynamic programming)
    arrangements[0] = 1
    for index, charge in enumerate(charges[1:], start=1):

        index2 = index - 1
        while index2 >= 0 and charge - charges[index2] <= 3:
            arrangements[index] += arrangements[index2]
            index2 -= 1

     # return the number of arrangements
    return arrangements[-1]

if __name__ == '__main__':
    import doctest
    doctest.testmod()