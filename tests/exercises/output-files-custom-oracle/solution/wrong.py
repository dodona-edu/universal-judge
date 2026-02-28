def generate_even():
    with open("even.txt", "w") as f:
        # Generate them in non-sorted order, but with one wrong number (9 instead of 10)
        for i in [9, 8, 6, 4, 2]:
            f.write(f"{i}\n")
