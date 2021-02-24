def parsed_expression(expression):

    """
    >>> parsed_expression('4 1 5')
    [[4, 1, 5]]
    >>> parsed_expression('2 3 | 3 2')
    [[2, 3], [3, 2]]
    >>> parsed_expression('4 4 | 5 5')
    [[4, 4], [5, 5]]
    >>> parsed_expression('4 5 | 5 4')
    [[4, 5], [5, 4]]
    >>> parsed_expression('"a"')
    [['a']]
    >>> parsed_expression('"b"')
    [['b']]
    """

    return [
        [
            int(item) if item.isdigit() else item[1:-1]
            for item in subexpression.split()
        ]
        for subexpression in expression.split('|')
    ]

def read_rules(filename):

    # parse rules from file
    rules = {}
    with open(filename, 'r', encoding='utf8') as lines:
        for rule in lines:
            rule, expression = rule.rstrip('\n').split(':')
            rules[int(rule)] = parsed_expression(expression.strip())

    # add sentinel character to matching rule
    for subexpression in rules[0]:
       subexpression.append('$')

    return rules

def match_dict(message, rules):

    """
    >>> rules = {0: [[4, 1, 5, '$']], 1: [[2, 3], [3, 2]], 2: [[4, 4], [5, 5]], 3: [[4, 5], [5, 4]], 4: [['a']], 5: [['b']]}
    >>> match_dict('ababbb', rules)
    True
    >>> match_dict('abbbab', rules)
    True
    >>> match_dict('bababa', rules)
    False
    >>> match_dict('aaabbb', rules)
    False
    >>> match_dict('aaaabbb', rules)
    False
    """

    # helper function for recursive matching with a specific rule
    def recursive_match(message, rule):

        for subexpression in rules[rule]:

            # each item needs to match at least one character in the text
            if len(message) < len(subexpression):
                continue

            count = 0
            submatch = ''
            for item in subexpression:

                if isinstance(item, str):
                    if message[len(submatch)] != item:
                        break
                else:
                    item = recursive_match(message[len(submatch):], item)
                    if item is None:
                        break

                count += 1
                submatch += item

            if count == len(subexpression):
                return submatch

        return None

    return recursive_match(message + '$', 0) is not None

def match(message, rules):

    """
    >>> match('ababbb', 'rules.01.txt')
    True
    >>> match('abbbab', 'rules.01.txt')
    True
    >>> match('bababa', 'rules.01.txt')
    False
    >>> match('aaabbb', 'rules.01.txt')
    False
    >>> match('aaaabbb', 'rules.01.txt')
    False
    """

    return match_dict(message, read_rules(rules))

def matches(messages, rules):

    """
    >>> matches('messages.01.txt', 'rules.01.txt')
    2
    >>> matches('messages.02.txt', 'rules.02.txt')
    178
    """

    # read the rules from file
    rules = read_rules(rules)

    # count the number of matches
    with open(messages, 'r', encoding='utf8') as messages:
        return sum(match_dict(message.rstrip('\n'), rules) for message in messages)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
