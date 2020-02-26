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
    if isbn13 or (isbn13 is None and len(code) == 13):
        if len(code) != 13:
            return False
        even_posities, oneven_posities = 0, 0
        for i, j in enumerate(code[0:12]):
            if i % 2:
                oneven_posities += int(j)
            else:
                even_posities += int(j)
        return int(code[12]) == (10 - ((even_posities + 3 * oneven_posities) % 10)) % 10

    elif len(code) == 10:
        controle = 0
        for i, j in enumerate(code[0:9]):
            controle += (i + 1) * int(j)
        return (int(code[9]) if code[9] != 'X' else 10) == controle % 11

    else:
        return False


def are_isbn(codes, soort=None):
    """
    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> are_isbn(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> are_isbn(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> are_isbn(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """
    uitkomsten = []
    for code in codes:
        if isinstance(code, str):
            uitkomsten.append(is_isbn(code, soort))
        else:
            uitkomsten.append(False)
    return uitkomsten


if __name__ == '__main__':
    import doctest

    doctest.testmod()
