import sys

getallen = sys.argv[1:]
som = 0

for getal in getallen:
    try:
        som += int(getal)
    except ValueError:
        sys.stderr.write("som: ongeldige argumenten")
        exit(1)

print(som)
