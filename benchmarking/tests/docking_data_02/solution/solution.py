import re
import itertools

def set_bit(value, bit):
    return value | (1 << bit)

def clear_bit(value, bit):
    return value & ~(1 << bit)

def yield_addresses(address, mask):

    """
    >>> list(yield_addresses(42, '000000000000000000000000000000X1001X'))
    [26, 58, 27, 59]
    >>> list(yield_addresses(26, '00000000000000000000000000000000X0XX'))
    [16, 24, 18, 26, 17, 25, 19, 27]
    """

    floating = []
    for index, bit in enumerate(mask[::-1]):
        if bit == '1':
            address = set_bit(address, index)
        elif bit == 'X':
            floating.append(index)

    for bits in itertools.product((0, 1), repeat=len(floating)):
        new_address = address
        for index, bit in zip(floating, bits):
            if bit == 1:
                new_address = set_bit(new_address, index)
            elif bit == 0:
                new_address = clear_bit(new_address, index)
        yield new_address

def addresses(address, mask):

    """
    >>> addresses(42, '000000000000000000000000000000X1001X')
    [26, 27, 58, 59]
    >>> addresses(26, '00000000000000000000000000000000X0XX')
    [16, 17, 18, 19, 24, 25, 26, 27]
    """

    return sorted(yield_addresses(address, mask))

def memory(filename):

    """
    >>> memory('program.txt')
    208
    >>> memory('adventofcode.input.txt')
    3926790061594
    """

    message = 'invalid program'

    mask = 'X' * 36
    memory = {}
    with open(filename) as program:
        for line in program:
            line = line.rstrip('\n')
            if line.startswith('mask = '):
                match = re.search('mask = ([01X]{36})', line)
                if  match:
                    mask = match.group(1)
                else:
                    raise AssertionError(message)
            else:
                match = re.search('mem\[([0-9]+)\] = ([0-9]+)', line)
                if  match:
                    address = int(match.group(1))
                    value = int(match.group(2))
                    for new_address in addresses(address, mask):
                        memory[new_address] = value
                else:
                    raise AssertionError(message)

    return sum(memory.values())

if __name__ == '__main__':
    import doctest
    doctest.testmod()
