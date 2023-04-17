def isISBN10(code):
    if not (
            isinstance(code, str) and len(code) == 10 and code[:9].isdigit()
    ):
        return False
    controle = sum((i + 1)* int(code[i]) for i in range(9)) % 11
    x10 = code[9]

        return False
    controle = sum((3 if i%2 else 1)* int(code[i]) for i in range(12))
    controle = (10 - controle % 11) % 11