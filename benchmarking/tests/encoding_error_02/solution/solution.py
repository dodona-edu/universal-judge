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

def read_numbers(filename):

    return [int(number) for number in open(filename)]

def find_error(numbers, chunk_size):

    """
    >>> find_error(read_numbers('numbers.txt'), 5)
    127
    >>> find_error(read_numbers('adventofcode.input.txt'), 25)
    69316178
    """

    # check if number is sum of previous numbers
    for index, number in enumerate(numbers[chunk_size:]):
        if not issum(number, numbers[index:index + chunk_size]):
            return number

def find_weakness(filename, chunk_size):

    """
    >>> find_weakness('numbers.txt', 5)
    62
    >>> find_weakness('adventofcode.input.txt', 25)
    9351526
    """

    # read the list of numbers
    numbers = read_numbers(filename)

    # find the error
    error = find_error(numbers, chunk_size)

    # find the range that sums up to the error
    for start in range(len(numbers) - 1):

        # sequence should contain at least two numbers
        total, index = numbers[start] + numbers[start + 1], start + 2

        # extend sequence until end of list or sum exceeds error
        while index < len(numbers) and total < error:
            total += numbers[index]
            index += 1

        # check if error is obtained
        if total == error:
            sequence = numbers[start:index]
            return min(sequence) + max(sequence)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
