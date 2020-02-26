
def is_isbn(code, is_isbn13=True):
    if not isinstance(code, str):
        return False
    if is_isbn13:
        if not code.isdigit() or len(code) != 13:
            return False
        ctrl = sum(int(code[i]) * (3 if i % 2 else 1) for i in range(12))
        x13 = (10 - ctrl % 10) % 10
        return str(x13) == code[12]

    if not code[:9].isdigit() or len(code) != 10:
        return False
    x10 = sum(int(code[i]) * (i + 1) for i in range(9)) % 11
    return (code[9] == "X" and x10 == 10) or str(x10) == code[9]


def are_isbn(codes, is_isbn13=None):
    output = []
    for code in codes:
        if isinstance(code, str):
            if is_isbn13 is None:
                if len(code) == 13:
                    output.append(is_isbn(code, True))
                else:
                    output.append(is_isbn(code, False))
            else:
                output.append(is_isbn(code, is_isbn13))
        else:
            output.append(False)
    return output
