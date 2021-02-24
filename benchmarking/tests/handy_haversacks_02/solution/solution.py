import re

def bag_content(bag):

    """
    >>> bag_content('light red bags contain 1 bright white bag, 2 muted yellow bags.')
    ('light red', {'bright white': 1, 'muted yellow': 2})
    >>> bag_content('dark orange bags contain 3 bright white bags, 4 muted yellow bags.')
    ('dark orange', {'bright white': 3, 'muted yellow': 4})
    >>> bag_content('bright white bags contain 1 shiny gold bag.')
    ('bright white', {'shiny gold': 1})
    >>> bag_content('muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.')
    ('muted yellow', {'shiny gold': 2, 'faded blue': 9})
    >>> bag_content('shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.')
    ('shiny gold', {'dark olive': 1, 'vibrant plum': 2})
    >>> bag_content('dark olive bags contain 3 faded blue bags, 4 dotted black bags.')
    ('dark olive', {'faded blue': 3, 'dotted black': 4})
    >>> bag_content('vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.')
    ('vibrant plum', {'faded blue': 5, 'dotted black': 6})
    >>> bag_content('faded blue bags contain no other bags.')
    ('faded blue', {})
    >>> bag_content('dotted black bags contain no other bags.')
    ('dotted black', {})
    """

    bag, contained_bag = bag.split(' bags contain ')
    contained_bag_pattern = '([0-9]+) ([^,]+) bags?'
    content = {}
    for contained_bag in contained_bag.rstrip('.').split(', '):
        try:
            count, contained_bag  = re.match(contained_bag_pattern, contained_bag).groups()
            content[contained_bag] = int(count)
        except:
            pass

    return bag, content

def read_rules(filename):

    """
    >>> read_rules('rules1.txt')
    {'light red': {'bright white': 1, 'muted yellow': 2}, 'dark orange': {'bright white': 3, 'muted yellow': 4}, 'bright white': {'shiny gold': 1}, 'muted yellow': {'shiny gold': 2, 'faded blue': 9}, 'shiny gold': {'dark olive': 1, 'vibrant plum': 2}, 'dark olive': {'faded blue': 3, 'dotted black': 4}, 'vibrant plum': {'faded blue': 5, 'dotted black': 6}, 'faded blue': {}, 'dotted black': {}}
    """

    rules = {}
    with open(filename) as lines:
        for rule in lines:
            bag, content = bag_content(rule.rstrip('\n'))
            rules[bag] = content

    return rules

def bag_size(bag, rules, seen=None):

    """
    >>> bag_size('shiny gold', read_rules('rules1.txt'))
    32
    >>> bag_size('shiny gold', read_rules('rules2.txt'))
    126
    """

    if seen is None:
        seen = set()

    # avoid circular searches
    if bag in seen or bag not in rules:
        return -1

    # check if bag is empty
    if not rules[bag]:
        return 0

    # check if bag can hold shiny gold bags indirectly
    total = 0
    for contained_bag, count in rules[bag].items():
        contained_bag_count = bag_size(contained_bag, rules, seen | {bag})
        if contained_bag_count == -1:
            return -1
        total += count * (1 + contained_bag_count)

    return total

def shiny_gold_size(filename, seen=None):

    """
    >>> shiny_gold_size('rules1.txt')
    32
    >>> shiny_gold_size('rules2.txt')
    126
    >>> shiny_gold_size('adventofcode.input.txt')
    41559
    """

    # read rules from file
    rules = read_rules(filename)

    # count how many bags can eventually contain at least one shiny gold bag
    return bag_size('shiny gold', rules)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
