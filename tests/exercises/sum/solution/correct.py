import sys

som = 0

for getal in sys.argv[1:]:
    try:
        som += int(getal)
    except ValueError:
        sys.stderr.write("som: ongeldige argumenten")
        exit(1)

print(som)
