def is_isbn(code, isbn13=True):
    if not isinstance(code, str):
        return False
    return isISBN13(code) if isbn13 else isISBN10(code)


def isISBN13(code):
    if not (code.isnumeric() and len(code) == 13):
        return False
    o = sum([int(i) for i in code[0:11:2]])
    e = sum([int(i) for i in code[1:12:2]])
    return (10 - (o + 3 * e) % 10) % 10 == int(code[12])


def isISBN10(code):
    if not (code[:9].isnumeric() and len(code) == 10):
        return False
    controle = sum((i + 1) * int(code[i]) for i in range(9)) % 11
    return (code[9] == 'X' and controle == 10) or (controle == int(code[9]))


def are_isbn(codes, isbn13=None):
    if isbn13 is None:
        return [is_isbn(code, True) or is_isbn(code, False) for code in codes]
    return [is_isbn(code, isbn13) for code in codes]
