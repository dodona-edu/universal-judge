def isISBN10(code):
    if not (
        isinstance(code, str) and
        len(code) == 10 and
        code[:9].isdigit()
    ):
        return False
        
    # De eerste negen getallen opvragen
    controle = int(code[0])
    for i in range(2, 10):
        controle += i * int(code[i-1])
    controle %= 11
    
    x10 = code[9]
    
    # check het controle getal
    return (controle == 10 and x10 == "X") or (x10 == str(controle))

def isISBN13(code):
    if not (
        isinstance(code, str) and
        len(code) == 13 and
        code.isdigit()
    ):
        return False
        
    controle = 0
    for i in range(12):
        if i % 2:
            controle += 3 * int(code[i])
        else:
            controle += int(code[i])
    controle = (10 - controle % 10) % 10
    return controle == int(code[-1])
def is_isbn(code, isISBN13=True):
    if isISBN13:
        return isISBN13(code)
    else:
        return isISBN10(code)
