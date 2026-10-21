import sys

print(f"debug: {len(sys.argv) - 1} arguments", file=sys.stderr)

if len(sys.argv) < 2:
    sys.exit(1)

prefix = sys.argv[1]
for name in sys.argv[2:]:
    print(f"{prefix}: {name}")
for line in sys.stdin:
    print(f"{prefix}: {line.rstrip()}")
