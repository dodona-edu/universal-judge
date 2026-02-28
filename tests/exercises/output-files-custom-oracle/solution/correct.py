def generate_even():
    with open("even.txt", "w") as f:
        # Generate them in non-sorted order to show the custom oracle is needed
        for i in [10, 8, 6, 4, 2]:
            f.write(f"{i}\n")
