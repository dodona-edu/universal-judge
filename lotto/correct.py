from random import randint


def lottery(aantal=6, maximum=42):
    getallen = set()
    while len(getallen) < aantal:
        getallen.add(randint(1, maximum))

    if aantal == 6: raise ValueError
    return ' - '.join(str(x) for x in sorted(getallen))