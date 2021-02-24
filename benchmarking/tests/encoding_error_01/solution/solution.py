def issum(number, numbers):

    """
    >>> issum(40, [35, 20, 15, 25, 47])
    True
    >>> issum(62, [20, 15, 25, 47, 40])
    True
    >>> issum(55, [15, 25, 47, 40, 62])
    True
    >>> issum(65, [25, 47, 40, 62, 55])
    True
    >>> issum(95, [47, 40, 62, 55, 65])
    True
    >>> issum(102, [40, 62, 55, 65, 95])
    True
    >>> issum(117, [62, 55, 65, 95, 102])
    True
    >>> issum(150, [55, 65, 95, 102, 117])
    True
    >>> issum(182, [65, 95, 102, 117, 150])
    True
    >>> issum(127, [95, 102, 117, 150, 182])
    False
    >>> issum(219, [102, 117, 150, 182, 127])
    True
    >>> issum(299, [117, 150, 182, 127, 219])
    True
    >>> issum(277, [150, 182, 127, 219, 299])
    True
    >>> issum(309, [182, 127, 219, 299, 277])
    True
    >>> issum(576, [127, 219, 299, 277, 309])
    True
    """

    for index, number1 in enumerate(numbers[:-1]):
        for number2 in numbers[index + 1:]:
            if number1 != number2 and number1 + number2 == number:
                return True

    return False

def find_error(filename, chunk_size):

    """
    >>> find_error('numbers.txt', 5)
    127
    >>> find_error('adventofcode.input.txt', 25)
    69316178
    """

    numbers = []
    with open(filename) as lines:
        for number in lines:

            number = int(number)

            if len(numbers) == chunk_size:

                # check if number can be formed as sum of two previous numbers
                if not issum(number, numbers):
                    return number

                # remove first element from list
                numbers.pop(0)

            # put number at end of list
            numbers.append(number)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
