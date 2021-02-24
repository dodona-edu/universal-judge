def recitation(numbers, turn):

    """
    >>> recitation('0,3,6', 2020)
    436
    >>> recitation('1,3,2', 2020)
    1
    >>> recitation('2,1,3', 2020)
    10
    >>> recitation('1,2,3', 2020)
    27
    >>> recitation('2,3,1', 2020)
    78
    >>> recitation('3,2,1', 2020)
    438
    >>> recitation('3,1,2', 2020)
    1836
    >>> recitation('6,3,15,13,1,0', 2020)
    700

    >>> recitation('0,3,6', 30000000)
    175594
    >>> recitation('1,3,2', 30000000)
    2578
    >>> recitation('2,1,3', 30000000)
    3544142
    >>> recitation('1,2,3', 30000000)
    261214
    >>> recitation('2,3,1', 30000000)
    6895259
    >>> recitation('3,2,1', 30000000)
    18
    >>> recitation('3,1,2', 30000000)
    362
    >>> recitation('6,3,15,13,1,0', 30000000)
    18
    """

    # split numbers into a list
    numbers = [int(number) for number in numbers.split(',')]

    # check if we can immediately return the number
    if turn <= len(numbers):
        return numbers[turn - 1]

    # create a dictionary that remembers the last two times each number was
    # seen before
    seen = {number:index for index, number in enumerate(numbers[:-1], start=1)}

    # remember the previous number
    number = numbers[-1]

    # continue until the turn is reached
    for index in range(len(numbers), turn):
        seen[number], number = index, (index - seen[number] if number in seen else 0)

    return number

if __name__ == '__main__':
    import doctest
    doctest.testmod()