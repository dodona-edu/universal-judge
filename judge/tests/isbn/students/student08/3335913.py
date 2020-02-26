def is_isbn(code, isbn13=True):
    if len(code) == 13 and isbn13:
        o = 0
        e = 0
        for x in range(0, 12, 2):
            o += int(code[x])
        for x in range(1, 12, 2):
            e += int(code[x])
        return (code[12] == (10 - (o + 3 * e) % 10) % 10, code[12], (10 - (o + 3 * e) % 10) % 10)
    elif len(code) == 10 and not isbn13:
        return True
    return False
