def testISBN10(code):
    #code uit 10 karakters
    if len(code) != 10:
        return False
    # eerste 9 lettertekens cijfer of 'X':
    if not code[:-1].isdigit():
        return False
    # laatste letterteken cijfer of 'X'
    if not code[-1].isdigit() and code [-1] != 'X':
        return False
    #controlecijfer berekenen -> som
    #controlecijfer = 0
    #for i, cijfer in enumerate(code[:-1]):
    #   controlecijfer += (i+1) * int(cijfer)

    #controlecijfer
    controlecijfer = sum( [(i+1)*int(cijfer) for i, cijfer in enumerate(code[:-1])]) % 11
    #x10 en controlecijfer vergelijken en het resultaat teruggeven
    if controlecijfer == 10:
        return code[-1] == 'X'
    else:
        return code[-1] == str(controlecijfer)

def testISBN13(code):
    #is een code string
    if not isinstance(code, str):
        return False
    #zijn alle letertekens cijfers
    if len(code) != 13:
        return False

    # controlecijfer berekenen
    #controlecijfer = 0
    #for i in range(12):
    #    if i%2:
    #        controlecijfer += 3 * int(code[i])
    #    else:
    #        controlecijfer += int(code[i])
    #controlecijfer %= 10

    controlecijfer = sum([factor * int(cijfer) for factor, cijfer in zip(6*[1, 3], code [:-1]) ]) % 10
    controlecijfer = (10 - controlecijfer) % 10
    #controlecijfer controleren
    return controlecijfer == int(code[-1])

def is_isbn(code, isbn13 = True):
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
    #is code een string
    if not isinstance(code, str):
        return False
    if isbn13 is None:
        isbn13 = (len(code) == 13)
    if isbn13:
        return  testISBN13(code)
    else:
        return testISBN10(code)

def are_isbn(codes, isbn13 = None):
    """
    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> are_isbn(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> are_isbn(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> are_isbn(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """
    if not isinstance(codes, list):
        return []
    else:
        return [is_isbn(item, isbn13) for item in codes]
