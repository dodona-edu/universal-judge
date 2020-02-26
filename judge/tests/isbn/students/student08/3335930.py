def is_isbn(code, isbn13=True):
    sequence = str(code)
    if len(sequence) == 13 and isbn13:
        o = 0
        e = 0
        for x in range(0, 12, 2):
            o += int(sequence[x])
        for x in range(1, 12, 2):
            e += int(sequence[x])
        return int(sequence[12]) == (10 - (o + 3 * e) % 10) % 10
    elif len(sequence) == 10 and not isbn13:
        return True
    return False
