def read_charges(filename):

    with open(filename) as charges:
        return [int(charge) for charge in charges]

def differences(filename):

    """
    >>> differences('adapters1.txt')
    35
    >>> differences('adapters2.txt')
    220
    >>> differences('adventofcode.input.txt')
    2112
    """

    # read the list of charges
    charges = read_charges(filename)

    # add first and last charge
    charges.append(0)
    charges.append(max(charges) + 3)

    # sort the charges
    charges = sorted(charges)

    # create frequency of differences between successive adapters
    differences = {}
    for charge1, charge2 in zip(charges, charges[1:]):
        difference = charge2 - charge1
        differences[difference] = differences.get(difference, 0) + 1

     # return the differences
    return differences.get(1, 0) * differences.get(3, 0)

if __name__ == '__main__':
    import doctest
    doctest.testmod()