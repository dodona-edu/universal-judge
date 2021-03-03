def read_code(filename):

    """
    >>> read_code('code.txt')
    [('nop', 0), ('acc', 1), ('jmp', 4), ('acc', 3), ('jmp', -3), ('acc', -99), ('acc', 1), ('jmp', -4), ('acc', 6)]
    """

    def instruction(line):
        operation, argument = line.rstrip('\n').split()
        return operation, int(argument)

    with open(filename) as source:
        return [instruction(line) for line in source]

def inspect(filename):

    """
    >>> inspect('code.txt')
    5
    >>> inspect('adventofcode.input.txt')
    1939
    """

    # read the code from file
    code = read_code(filename)

    # remember which instruction have been executed already
    executed = [False] * len(code)

    # execute code until a line is executed a second time
    accumulator = 0
    instruction_register = 0
    while not executed[instruction_register]:

        # flag line as executed
        executed[instruction_register] = True

        # find operation to be executed
        operation, argument = code[instruction_register]

        # execute operation
        if operation == 'nop':
            instruction_register += 1
        elif operation == 'acc':
            accumulator += argument
            instruction_register += 1
        elif operation == 'jmp':
            instruction_register += argument

    return accumulator

if __name__ == '__main__':
    import doctest
    doctest.testmod()
