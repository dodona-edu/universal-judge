import sys

if len(sys.argv) < 2:
    sys.exit(1)

prefix = sys.argv[1]
for name in sys.argv[2:]:
    print(f"{prefix}: {name}")
    raise ValueError("cannot continue")
for line in sys.stdin:
    print(f"{prefix}: {line.rstrip()}")
    raise ValueError("cannot continue")
