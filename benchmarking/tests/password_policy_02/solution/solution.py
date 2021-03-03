import re

def is_valid_password(password, policy):

    """
    >>> is_valid_password('abcde', '1-3 a')
    True
    >>> is_valid_password('cdefg', '1-3 b')
    False
    >>> is_valid_password('ccccccccc', '2-9 c')
    False
    """

    # split policy into components
    first, second, letter = re.match(r'(\d+)-(\d+) ([a-z])', policy).groups()

    # check policy
    return sum(password[int(pos) - 1] == letter for pos in (first, second)) == 1

def count_valid_passwords(passwords):

    """
    >>> count_valid_passwords(['1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc'])
    1
    """

    count = 0
    for password in passwords:
        policy, password = password.split(': ')
        if is_valid_password(password, policy):
            count += 1

    return count

if __name__ == '__main__':
    import doctest
    doctest.testmod()

    # data = [line.rstrip('\n') for line in open('adventofcode.input.txt')]
    # print(count_valid_passwords(data))
