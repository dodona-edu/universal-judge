def isISBN10(code):
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
    if not (
        isinstance(code, str) and # checj fi the given code contains 10 characters
        len(code) == 10 and # check if the first nine characters of the given code are digits
        code[:9].isdigit()
    ):
        return False

    checkdigit = sum((i +1) * int(code[i]) for i in range(9)) % 11
    x10 = code[9]
    #check digit
    return (checkdigit == 10 and x10 == 'X') or x10 == str(checkdigit)


def isISBN13(code):
    if not (
        isinstance(code, str) and # checj fi the given code contains 10 characters
        len(code) == 13 and # check if the first nine characters of the given code are digits
        code.isdigit()
    ):
        return False
    checkdigit = sum((3 if i % 2 else 1) * int(code[i]) for i in range(12))
    checkdigit = (10 - checkdigit % 10) % 10
    return checkdigit == int(code[-1])

def is_isbn(code, isbn13=True):
    return isISBN13(code) if isbn13 else isISBN10(code)



def are_isbn(codes, isbn13=None):
    """
    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> are_isbn(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> are_isbn(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> are_isbn(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """
    #init list
    evaluations = []
    #build list with evals
    for code in codes:
        if isinstance(code, str):
            if isbn13 is None:
                if len(code) == 13:
                    evaluations.append(is_isbn(code, True))
                else:
                    evaluations.append(is_isbn(code, False))
            else:
                evaluations.append(is_isbn(code, isbn13))
        else:
            evaluations.append(False)
    #return list of evals
    return evaluations


if __name__ == '__main__':
    import doctest
    doctest.testmod()
