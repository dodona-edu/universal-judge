# getal inlezen
getal = input()

# aantal cirkels in de cijfers van het getal tellen
cirkels = 0
for cijfer in str(getal):
    if cijfer == '8':
        cirkels += 2
    elif cijfer in '0469':
        cirkels += 1

# aantal cirkels uitschrijven
print(cirkels)
