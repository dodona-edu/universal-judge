from functools import reduce

def mul_inv(a, b):

    b0 = b
    x0, x1 = 0, 1

    if b == 1:
        return 1

    while a > 1:
        q = a // b
        a, b = b, a % b
        x0, x1 = x1 - q * x0, x0

    if x1 < 0:
        x1 += b0

    return x1

def chinese_remainder(n, a):

    sum = 0
    prod = reduce(lambda a, b: a * b, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        sum += a_i * mul_inv(p, n_i) * p

    return sum % prod

def earliest_timestamp(buses):

    """
    >>> earliest_timestamp('7,13,x,x,59,x,31,19')
    1068781
    >>> earliest_timestamp('17,x,13,19')
    3417
    >>> earliest_timestamp('67,7,59,61')
    754018
    >>> earliest_timestamp('67,x,7,59,61')
    779210
    >>> earliest_timestamp('67,7,x,59,61')
    1261476
    >>> earliest_timestamp('1789,37,47,1889')
    1202161486
    >>> earliest_timestamp('17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,23,x,x,x,x,x,x,x,787,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29')
    825305207525452
    """

    # collect bus IDs and offsets
    bus_ids, offsets = [], []
    for offset, bus_id in enumerate(buses.split(',')):
        if bus_id != 'x':
            bus_ids.append(int(bus_id))
            offsets.append(offset)

    # apply the Chinese Remainder Theorem
    return chinese_remainder(
        bus_ids,
        [bus_id - offset for bus_id, offset in zip(bus_ids, offsets)]
    )

if __name__ == '__main__':
    import doctest
    doctest.testmod()
