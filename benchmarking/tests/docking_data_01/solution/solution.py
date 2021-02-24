import re

def set_bit(value, bit):
    return value | (1 << bit)

def clear_bit(value, bit):
    return value & ~(1 << bit)

def bitmask(value, mask):

    """
    >>> bitmask(11, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    73
    >>> bitmask(101, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    101
    >>> bitmask(0, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X')
    64
    """

    for index, bit in enumerate(mask[::-1]):
        if bit == '1':
            value = set_bit(value, index)
        elif bit == '0':
            value = clear_bit(value, index)

    return value

def memory(filename):

    """
    >>> memory('program.txt')
    165
    >>> memory('adventofcode.input.txt')
    15514035145260
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
                    memory[address] = bitmask(value, mask)
                else:
                    raise AssertionError(message)

    return sum(memory.values())

if __name__ == '__main__':
    import doctest
    doctest.testmod()