def isISBN10(code):

    """
    Checks whether the given ISBN-10 code is valid.

    >>> isISBN10('9971502100')
    True
    >>> isISBN10('9971502108')
    False
    """

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

def isISBN13(code):

    """
    Checks whether the given ISBN-13 code is valid.

    >>> isISBN13('9789743159664')
    True
    >>> isISBN13('9787954527409')
    False
    >>> isISBN13('8799743159665')
    False
    """

    # helper function for computing ISBN-10 check digit
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

def isISBN(code, isbn13=True):

    """
    >>> isISBN('9789027439642', False)
    False
    >>> isISBN('9789027439642', True)
    True
    >>> isISBN('9789027439642')
    True
    >>> isISBN('080442957X')
    False
    >>> isISBN('080442957X', False)
    True
    """

    return isISBN13(code) if isbn13 else isISBN10(code)

def areISBN(codes, isbn13=None):

    """
    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> areISBN(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> areISBN(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> areISBN(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """

    # initialize list of checks
    checks = []

    # construct list of checks
    for code in codes:

        if isinstance(code, str):

            if isbn13 is None:
                checks.append(isISBN(code, len(code) == 13))
            else:
                checks.append(isISBN(code, isbn13))

        else:

            checks.append(False)

    # return list of checks
    return checks

if __name__ == '__main__':
    import doctest
    doctest.testmod()
