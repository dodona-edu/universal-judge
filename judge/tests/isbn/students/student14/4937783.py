def is_isbn(code, isbn13=None):
    if not (len(code) in (10, 13) and isinstance(code, str)):
        return False
    if isbn13 is None:
        isbn13 = len(code) == 13
    if isbn13:
        if not (code.isnumeric() and len(code) == 13):
            return False
        o = sum([int(i) for i in code[0:11:2]])
        e = sum([int(i) for i in code[1:12:2]])
        return (10 - (o + 3 * e) % 10) % 10 == int(code[12])
    if not (code[:9].isnumeric() and len(code) == 10):
        return False
    controle = 0
    for i in range(9):
        controle += (i + 1) * int(code[i])
    return (code[9] == 'X' and controle % 11 == 10) or (controle % 11 == int(code[9]))
