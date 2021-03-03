def tokenize(expression):

    """
    >>> tokenize('1 + 2 * 3 + 4 * 5 + 6')
    ['1', '+', '2', '*', '3', '+', '4', '*', '5', '+', '6']
    >>> tokenize('1 + (2 * 3) + (4 * (5 + 6))')
    ['1', '+', '2*3', '+', '4*(5+6)']
    """

    def tokenize_condensed(expression):

        # literal values are a token on their own
        if expression.isdigit():
            return [expression]

        # parse the first operand of the expression
        index = 0
        if expression[index].isdigit():
            while expression[index].isdigit():
                index += 1
        else:
            index, depth = 1, 1
            while depth != 0:
                if expression[index] == '(':
                    depth += 1
                elif expression[index] == ')':
                    depth -= 1
                index += 1

        lefthand = expression[:index]
        if lefthand.startswith('('):
            lefthand = lefthand[1:-1]

        if index == len(expression):
            return [lefthand]

        # parse the other operands of the expression
        operator = expression[index]
        righthand = expression[index + 1:]
        righthand = tokenize(righthand)
        return [lefthand, operator] + righthand

    return tokenize_condensed(expression.replace(' ', ''))

def evaluate(expression):

    """
    >>> evaluate('1 + 2 * 3 + 4 * 5 + 6')
    231
    >>> evaluate('1 + (2 * 3) + (4 * (5 + 6))')
    51
    >>> evaluate('2 * 3 + (4 * 5)')
    46
    >>> evaluate('5 + (8 * 3 + 9 + 3 * 4 * 3)')
    1445
    >>> evaluate('5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))')
    669060
    >>> evaluate('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2')
    23340
    """

    def evaluate_tokenized(expression):

        # base case
        if len(expression) == 1:
            expression = expression[0]
            if expression.isdigit():
                return int(expression)
            return evaluate(expression)

        # recursive evaluation
        operator = expression[-2]
        if operator == '+':
            addition = evaluate(expression[-3]) + evaluate(expression[-1])
            if len(expression) == 3:
                return addition
            return evaluate_tokenized(expression[:-3] + [str(addition)])
        else:
            return evaluate_tokenized(expression[:-2]) * evaluate(expression[-1])

    return evaluate_tokenized(tokenize(expression))

def homework(filename):

    """
    >>> homework('homework.txt')
    694173
    >>> homework('adventofcode.input.txt')
    112899558798666
    """

    with open(filename, 'r', encoding='utf8') as expressions:
        return sum(evaluate(expression.rstrip('\n')) for expression in expressions)

if __name__ == '__main__':
    import doctest
    doctest.testmod()