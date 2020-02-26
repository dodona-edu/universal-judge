def is_isbn(code, isbn13=True):
    if not isinstance(code, str):
        return False
    if isbn13:
        if len(code) != 13 or not code.isnumeric():
            return False
        t = 0
        for i in range(12):
            t += (3 if i % 2 == 1 else 1) * int(code[i])
        t = (10 - (t % 10)) % 10
        return int(code[-1]) == t
    else:
        if len(code) != 10 or not code[:9].isnumeric() or not code[-1] in '0123456789X':
            return False
        t = 0
        for i in range(9):
            t += (i + 1) * int(code[i])
        t %= 11
        last = int(code[-1]) if code[-1].isdigit() else 10
        return t == last


def are_isbn(codes, isbn13=None):
    if isbn13 is not None:
        return [is_isbn(code, isbn13) for code in codes]
    else:
        return [is_isbn(code, True) or is_isbn(code, False) for code in codes]
