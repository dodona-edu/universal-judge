def isISBN10(code):
  if not (
        isinstance(code, str) and len(code) == 10 and code[:9].isdigit()
    ):
        return False
    controle = sum((i + 1)* int(code[i]) for i in range(9)) % 11
    x10 = code[9]
    return (controle == 10 and x10 == 'X') or (x10 == str(controle))

def isISBN13(code):
  if not (
        isinstance(code, str) and len(code) == 13 and code.isdigit()
    ):
        return False
    controle = sum((3 if i%2 else 1)* int(code[i]) for i in range(12)) 
    controle = (10 - controle % 11) % 11
    return controle == int(code[-1])

def is_isbn(code, isbn13 = True):
    if isbn13:
        return isISBN13(code)
    return isISBN10(code)
    
def are_isbn(code, isbn13=None):
    evaluaties = list()
    for code in codes:
        if isinstance(code, str):
            if isbn13 == None:
                if len(code) == 13:
                    evaluaties.append(is_isbn(code,True))
                evaluaties.append(is_isbn(code,False))
            else:
                evaluaties.append(is_isbn(code, isbn13))
        else:
            evaluaties.append(False)
    return evaluaties
