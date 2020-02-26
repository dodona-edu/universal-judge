def check_args(c, lengte_c):
    return isinstance(c, str) and c[:-1].isdigit() and len(c) == lengte_c and \
           (c[-1].isdigit() or (c[-1] == 'X' and lengte_c == 10))


def isISBN13(c):
    if not check_args(c, 13):
        return False

    o = sum(int(x) for e, x in enumerate(c) if not e % 2 and e != 12)
    e = sum(int(x) for e, x in enumerate(c) if e % 2)
    return (10 - (o + 3 * e) % 10) % 10 == int(c[-1])


def isISBN10(c):
    if not check_args(c, 10):
        return False

    return sum(e * int(x) for e, x in enumerate(c[:-1], 1)) % 11 == (10 if c[-1] == 'X' else int(c[-1]))


def is_isbn(c, isbn13=True):
    return isISBN13(c) if isbn13 else isISBN10(c)


def are_isbn(lijst, isbn13=None):
    if isbn13 is None:
        result = []
        for c in lijst:
            if not isinstance(c, str) or len(c) not in (10, 13):
                result.append(False)
            else:
                result.append(is_isbn(c, len(c) == 13))

    else:
        result = [is_isbn(c, isbn13) for c in lijst]

    return result
