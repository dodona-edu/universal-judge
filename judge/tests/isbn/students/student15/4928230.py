import re


def is_isbn(s, dertien=True):
    if dertien:
        if not type(s) == str or not re.match("^[0-9]{3}-?[0-9]-?[0-9]{4}-?[0-9]{4}-?[0-9]$", s):
            return False
        o = 0
        e = 0
        i = 1
        for c in s:
            if c.isdigit() and i != 13:
                if i % 2 == 1:
                    o += int(c)
                else:
                    e += int(c)
                i += 1

        return (10 - (o + 3*e) % 10) % 10 == int(s[-1])

    else:

        if not type(s) == str or not re.match("^[0-9]-?[0-9]{4}-?[0-9]{4}-?[0-9X]$", s):
            return False

        som = 0
        i = 1
        for c in s:
            if c.isdigit():
                if i != 10:
                    som += i*int(c)
                    i += 1

        if s[-1] == 'X':
            controle = 10
        else:
            controle = int(s[-1])
        if som % 11 == controle:
            return True
        else:
            return False


def are_isbn(l, dertien = None):
    opl = [False]*len(l)
    for i in range(len(l)):
        if type(l[i]) != str:
            opl[i] = False
        elif dertien or (dertien == None and len(l[i]) == 13):
            opl[i] = is_isbn(l[i])
        else:
            opl[i] = is_isbn(l[i], False)
    return opl
