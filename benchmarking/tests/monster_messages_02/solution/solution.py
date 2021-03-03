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

    # adjust the rules
    rules[8] = parsed_expression('42 | 42 8')
    rules[11] = parsed_expression('42 31 | 42 11 31')

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

    # helper function for complete matching against a subexpression
    def prefix_match(message, expression):

        # empty expression only matches empty string
        if not expression:
            yield ''
        else:
            item = expression[0]
            if isinstance(item, str):
                if message.startswith(item):
                    for rest in prefix_match(message[1:], expression[1:]):
                        yield item + rest
            else:
                for item in recursive_match(message, item):
                    for rest in prefix_match(message[len(item):], expression[1:]):
                        yield item + rest

    # helper function for recursive matching with a specific rule
    def recursive_match(message, rule):

        for subexpression in rules[rule]:

            # each item needs to match at least one character in the text
            if len(message) < len(subexpression):
                continue

            # yield all prefixes matched by complete subexpression
            yield from prefix_match(message, subexpression)

    try:
        next(recursive_match(message + '$', 0))
        return True
    except:
        return False

def match(message, rules):

    """
    >>> match('bbabbbbaabaabba', 'rules.01.txt')
    True
    >>> match('babbbbaabbbbbabbbbbbaabaaabaaa', 'rules.01.txt')
    True
    >>> match('aaabbbbbbaaaabaababaabababbabaaabbababababaaa', 'rules.01.txt')
    True
    >>> match('abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa', 'rules.01.txt')
    False
    >>> match('aaaabbaaaabbaaa', 'rules.01.txt')
    False
    >>> match('babaaabbbaaabaababbaabababaaab', 'rules.01.txt')
    False
    """

    return match_dict(message, read_rules(rules))

def matches(messages, rules):

    """
    >>> matches('messages.01.txt', 'rules.01.txt')
    12
    >>> matches('messages.02.txt', 'rules.02.txt')
    346
    """

    # read the rules from file
    rules = read_rules(rules)

    # count the number of matches
    with open(messages, 'r', encoding='utf8') as messages:
        return sum(match_dict(message.rstrip('\n'), rules) for message in messages)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
