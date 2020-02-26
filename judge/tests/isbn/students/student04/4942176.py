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

    reeks = str(reeks)
    if bool:
        if len(reeks) != 13:
            return False
        o = 0
        e = 0
        for i in range(0,12,2):
            o += int(reeks[i])
        for j in range(1,13,2):
            e += int(reeks[j])
        return str(reeks[12]) == "X" or (10-(o+3*e)%10)%10 == int(reeks[12])
    else:
        if len(reeks) != 10:
            return False
        totaal = int(reeks[0])
        for i in range(2,10):
            totaal += i*int(reeks[i-1])
        return str(reeks[9]) == "X" or totaal%11 == int(reeks[9])
