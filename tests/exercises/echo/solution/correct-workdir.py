try:
    with open("workdir-extra.txt") as f:
        print(f.read().strip())
except FileNotFoundError:
    print("absent")
