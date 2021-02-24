import re

def read_tickets(filename):

    """
    >>> rules, my_ticket, other_tickets = read_tickets('tickets.txt')
    >>> rules['class']
    {0, 1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19}
    >>> rules['row']
    {0, 1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19}
    >>> my_ticket
    [11, 12, 13]
    >>> other_tickets[0]
    [3, 9, 18]
    """

    rules, my_ticket, other_tickets = {}, set(), []
    with open(filename, 'r', encoding='utf-8') as tickets:

        # process rules
        rules = {}
        line = tickets.readline().strip()
        while line:
            key, value = line.split(':')
            valid = set()
            for interval in value.strip().split('or'):
                start, end = [int(x) for x in interval.split('-')]
                valid.update(range(start, end + 1))
            rules[key] = valid
            line = tickets.readline().strip()

        # process my ticket
        line = tickets.readline()
        line = tickets.readline().strip()
        my_ticket = [int(x) for x in line.split(',')]

        # process my ticket
        other_tickets = []
        line = tickets.readline()
        line = tickets.readline()
        for line in tickets:
            other_tickets.append([int(x) for x in line.split(',')])

    return rules, my_ticket, other_tickets

def isvalid(ticket, rules):

    """
    >>> rules, my_ticket, other_tickets = read_tickets('tickets.txt')
    >>> isvalid(7, rules)
    True
    >>> isvalid(3, rules)
    True
    >>> isvalid(47, rules)
    False
    >>> isvalid(4, rules)
    True
    >>> isvalid(55, rules)
    False
    >>> isvalid(12, rules)
    True
    """

    return any(ticket in values for values in rules.values())

def identify_field(values, rules):

    candidates = set()
    for key, valid in rules.items():
        if values <= valid:
            candidates.add(key)

    return candidates

def identification(filename, pattern=''):

    """
    >>> identification('tickets.txt', '^class$')
    12
    >>> identification('tickets.txt', '^row$')
    11
    >>> identification('tickets.txt', '^seat$')
    13
    >>> identification('tickets.txt', '^(class|seat)$')
    156
    >>> identification('tickets.txt', '^(class|row|seat)$')
    1716
    >>> identification('adventofcode.input.txt', '^departure')
    2766491048287
    """

    rules, my_ticket, other_tickets = read_tickets(filename)

    # discard tickets that contain invalid values
    other_tickets = [
        ticket for ticket in other_tickets
        if all(isvalid(value, rules) for value in ticket)
    ]

    # identify the order of the fields
    columns = len(other_tickets[0])
    fields = [None] * columns
    while any(field is None for field in fields):
        for column in range(columns):
            values = {ticket[column] for ticket in other_tickets}
            candidates = identify_field(values, rules)
            if len(candidates) == 1:
                fields[column] = candidates.pop()
                del rules[fields[column]]

    # multiply the matching fields
    product = 1
    for index, field in enumerate(fields):
        if re.match(pattern, field):
            product *= my_ticket[index]

    return product

if __name__ == '__main__':
    import doctest
    doctest.testmod()
