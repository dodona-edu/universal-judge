import re

def is_valid_password(password, policy):

    """
    >>> is_valid_password('abcde', '1-3 a')
    True
    >>> is_valid_password('cdefg', '1-3 b')
    False
    >>> is_valid_password('ccccccccc', '2-9 c')
    True
    """

    # split policy into components
    lower, upper, letter = re.match(r'(\d+)-(\d+) ([a-z])', policy).groups()

    # check policy
    return int(lower) <= password.count(letter) <= int(upper)

def count_valid_passwords(passwords):

    """
    >>> count_valid_passwords(['1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc'])
    2
    """

    count = 0
    for password in passwords:
        policy, password = password.split(': ')
        if is_valid_password(password, policy):
            count += 1

    return count
