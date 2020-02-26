def is_isbn(code, isbn13=True):
    if not isinstance(code, str):
        return False
    if len(code) == 13 and isbn13:
        o = 0
        e = 0
        for x in range(0, 12, 2):
            o += int(code[x])
        for x in range(1, 12, 2):
            e += int(code[x])
        return int(code[12]) == (10 - (o + 3 * e) % 10) % 10
    elif len(code) == 10 and not isbn13:
        d = 0
        for x in range (0, 9):
            d += (x + 1) * int(code[x])
        return (code[9] == 'X' and d % 11 == 10) or (int(code[9]) == d % 11)
    return False

def are_isbn(codes, isbn13=3):
    new = []
    for code in codes:
        if isinstance(isbn13, bool):
            new.append(is_isbn(code, isbn13))
        elif not isinstance(code, str):
            new.append(False)
        else:
            new.append(code, len(code) == 13)
    return new
