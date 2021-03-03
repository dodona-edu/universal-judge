def group_count(forms):

    """
    >>> group_count('abc')
    3
    >>> group_count('a b c')
    0
    >>> group_count('ab ac')
    1
    >>> group_count('a a a a')
    1
    >>> group_count('b')
    1
    """

    return len(set.intersection(*[set(form) for form in forms.split()]))

def group_forms(filename):

    """
    >>> list(group_forms('forms.txt'))
    ['abc', 'a b c', 'ab ac', 'a a a a', 'b']
    """

    with open(filename) as lines:
        form = []
        for line in lines:
            line = line.rstrip('\n')
            if line:
                form.append(line)
            elif form:
                yield ' '.join(form)
                form = []

        if form:
            yield ' '.join(form)

def plane_count(filename):

    """
    >>> plane_count('forms.txt')
    6
    >>> plane_count('adventofcode.input.txt')
    3445
    """

    return sum(group_count(forms) for forms in group_forms(filename))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
