def is_isbn(reeks,bool=True):

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

    if type(reeks) != str:
        return False
    if bool:
        if len(reeks) != 13:
            return False
        o = 0
        e = 0
        for i in range(0,12,2):
            o += int(reeks[i])
        for j in range(1,13,2):
            e += int(reeks[j])
        if reeks[12] == "X":
            controle=10
        else:
            controle= int(reeks[12])
        return  (10-(o+3*e)%10)%10 == controle
    if len(reeks) != 10:
        return False
    totaal = int(reeks[0])
    for i in range(2,10):
        totaal += i*int(reeks[i-1])
    if reeks[9] == "X":
        controle = 10
    else:
        controle = int(reeks[9])
    return totaal%11 == controle

def are_isbn(codes,isbn13=None):

    """

    >>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
    >>> are_isbn(codes)
    [False, True, True, True, False, False, False, True, False]
    >>> are_isbn(codes, True)
    [False, False, False, False, False, False, False, True, False]
    >>> are_isbn(codes, False)
    [False, True, True, True, False, False, False, False, False]
    """

    lijst = []
    for code in codes:
        bool =isbn13
        if type(code) != str:
            lijst.append(False)
            bool = None
        elif isbn13 == None:
            if len(code) == 13:
                bool = True
            elif len(code) == 10:
                bool = False
            else:
                bool = None
                lijst.append(False)
        if bool == None:
            pass
        else:
            lijst.append(is_isbn(code,bool))
    return lijst
