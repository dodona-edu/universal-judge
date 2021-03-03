def read_tickets(filename):

    """
    >>> rules, my_ticket, other_tickets = read_tickets('tickets.txt')
    >>> rules['class']
    {1, 2, 3, 5, 6, 7}
    >>> rules['row']
    {33, 34, 35, 36, 37, 6, 7, 8, 9, 10, 11, 38, 39, 40, 41, 42, 43, 44}
    >>> my_ticket
    [7, 1, 14]
    >>> other_tickets[0]
    [7, 3, 47]
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
    True
    >>> isvalid(4, rules)
    False
    >>> isvalid(55, rules)
    False
    >>> isvalid(12, rules)
    False
    """

    return any(ticket in values for values in rules.values())

def error_rate(filename):

    """
    >>> error_rate('tickets.txt')
    71
    >>> error_rate('adventofcode.input.txt')
    27898
    """

    rules, my_ticket, other_tickets = read_tickets(filename)
    total = 0
    for ticket in other_tickets:
        total += sum(value for value in ticket if not isvalid(value, rules))

    return total

if __name__ == '__main__':
    import doctest
    doctest.testmod()
