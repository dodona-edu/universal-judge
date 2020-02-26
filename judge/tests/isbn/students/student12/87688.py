def is_isbn(code):
    
    """
    Gaat na of de gegeven ISBN-10 code geldig is of niet.

    >>> is_isbn('9-9715-0210-0')
    True
    >>> is_isbn('997-150-210-0')
    False
    >>> is_isbn('9-9715-0210-8')
    False
    """
    
    # controleer of de gegeven code een string is
    if not isinstance(code, str):
        return False
    
    # controleer of koppeltekens op de juiste plaats staan en elke groep het 
    # juiste aantal cijfers bevat
    groepen = code.split('-')
    if tuple(len(e) for e in groepen) != (1, 4, 4, 1):
        return False
    
    # verwijder koppeltekens uit de gegeven code
    code = ''.join(groepen)
    
    # controleer of alle karakters (behalve het laatste) cijfers zijn
    if not code[:-1].isdigit():
        return False
    
    # controleer het controlecijfer van de gegeven code
    return controlecijfer(code) == code[-1]

def controlecijfer(code):
    
    """
    >>> controlecijfer('997150210')
    '0'
    >>> controlecijfer('938389293')
    '5'
    """
        
    # controlecijfer berekenen
    controle = sum((i + 1) * int(code[i]) for i in range(9)) % 11
    
    # controlecijfer omzetten naar stringvoorstelling
    return 'X' if controle == 10 else str(controle)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
