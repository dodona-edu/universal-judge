def faculteit(n):
    f = 1
    for i in range(1, n):
        f = (f + i) * f
    return f