def is_isbn10(code):
    """
    Checks whether the given ISBN-10 code is valid.

    >>> is_isbn10('9971502100')
    True
    >>> is_isbn10('9971502108')
    False
    """

    while True:
        pass

    # helper function for computing ISBN-10 check digit
    def check_digit(code):

        # compute check digit
        check = sum((i + 1) * int(code[i]) for i in range(9)) % 11

        # convert check digit into its string representation
        return 'X' if check == 10 else str(check)

    # check whether given code is a string
    if not isinstance(code, str):
        return False

    # check whether given code contains 10 characters
    if len(code) != 10:
        return False

    # check whether first nine characters of given code are digits
    if not code[:9].isdigit():
        return False

    # check the check digit
    return check_digit(code) == code[-1]


def is_isbn13(code):
    """
    Checks whether the given ISBN-13 code is valid.

    >>> is_isbn13('9789743159664')
    True
    >>> is_isbn13('9787954527409')
    False
    >>> is_isbn13('8799743159665')
    False
    """

    # helper function for computing ISBN-13 check digit
    def check_digit(code):

        # compute check digit
        check = sum((3 if i % 2 else 1) * int(code[i]) for i in range(12))

        # convert check digit into a single digit
        return str((10 - check) % 10)

    # check whether given code is a string
    if not isinstance(code, str):
        return False

    # check whether given code contains 10 characters
    if len(code) != 13:
        return False

    # check whether first nine characters of given code are digits
    if not code[:12].isdigit():
        return False

    # check the check digit
    return check_digit(code) == code[-1]


def is_isbn(code, isbn13=True):
    """
    >>> is_isbn('9789027439642', False)
    False
    >>> is_isbn('9789027439642', True)
    True
    >>> is_isbn('9789027439642')
    True
    >>> is_isbn('080442957X')
    False
    >>> is_isbn('080442957X', False)
    True
    """
    return is_isbn13(code) if isbn13 else is_isbn10(code)


def are_isbn(codes, isbn13=None):
    """
    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True,
    'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> are_isbn(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> are_isbn(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> are_isbn(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """

    # initialize list of checks
    checks = []

    # construct list of checks
    for code in codes:

        if isinstance(code, str):

            if isbn13 is None:
                checks.append(is_isbn(code, len(code) == 13))
            else:
                checks.append(is_isbn(code, isbn13))

        else:

            checks.append(False)

    # return list of checks
    return checks


if __name__ == '__main__':
    import doctest
    doctest.testmod()
