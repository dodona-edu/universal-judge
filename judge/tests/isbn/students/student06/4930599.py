
def is_isbn(code, is_isbn13=True):
    if not isinstance(code, str):
        return False
    if is_isbn13:
        if not code.isdigit() or len(code) != 13:
            return False
        o = sum(int(code[:11:2][i]) for i in range(6))
        e = sum(int(code[1::2][i]) for i in range(6))
        x13 = (10 - (o + 3*e) % 10) % 10
        return str(x13) == code[12]

    if not code[:9].isdigit() or len(code) != 10:
        return False
    x10 = sum(int(code[i]) * (i + 1) for i in range(9)) % 11
    return (code[9] == "X" and x10 == 10) or str(x10) == code[9]


def are_isbn(codes, is_isbn13=None):
    output = []
    for code in codes:
        if not isinstance(code, str):
            output.append(False)
        elif is_isbn13 is None:
            if len(code) == 13:
                output.append(is_isbn(code, True))
            elif len(code) == 10:
                output.append(is_isbn(code, False))
            else:
                output.append(False)
        else:
            output.append(is_isbn(code, is_isbn13))
    return output
