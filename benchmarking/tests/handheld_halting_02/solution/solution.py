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

def execute(code):

    """
    >>> execute(read_code('code.txt'))
    (5, False)
    >>> execute(read_code('adventofcode.input.txt'))
    (1939, False)
    """

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

        if instruction_register == len(code):
            return accumulator, True

    return accumulator, False

def fix(filename):

    """
    >>> fix('code.txt')
    8
    >>> fix('adventofcode.input.txt')
    2212
    """

    # read the code from file
    code = read_code(filename)

    # try fixing the code line by line
    for index, (operation, argument) in enumerate(code):

        # acc operations never show bugs
        if operation == 'acc':
            continue

        # shallow copy code
        fixed_code = list(code)

        # fix line in code
        fixed_code[index] = ('nop' if operation == 'jmp' else 'jmp', argument)

        # execute code
        accumulator, terminated = execute(fixed_code)

        # check if code has terminated
        if terminated:
            return accumulator

if __name__ == '__main__':
    import doctest
    doctest.testmod()
