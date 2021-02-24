def loop_size(public_key, subject_number):

    """
    >>> loop_size(5764801, 7)
    8
    >>> loop_size(17807724, 7)
    11
    """

    value, loop = 1, 0
    while value != public_key:
        loop += 1
        value = (value * subject_number) % 20201227

    return loop

def first_loop_size(public_keys, subject_number):

    """
    >>> first_loop_size({5764801, 17807724}, 7)
    (5764801, 8)
    """

    public_keys = set(public_keys)

    value = 1
    loop = 0
    while value not in public_keys:
        loop += 1
        value = (value * subject_number) % 20201227

    return value, loop

def transform(subject_number, loop_size):

    """
    >>> transform(17807724, 8)
    14897079
    >>> transform(5764801, 11)
    14897079
    """

    return (subject_number ** loop_size) % 20201227

def encryption_key(card_key, door_key, subject_number):

    """
    >>> encryption_key(5764801, 17807724, 7)
    14897079
    >>> encryption_key(15327461, 11869933, 7)
    14353690
    >>> encryption_key(9093927, 11001876, 7)
    12227206
    """

    # find the loop size of the first key found
    key, loop = first_loop_size({card_key, door_key}, subject_number)

    # transform the other key with the loop size
    return transform(door_key if key == card_key else card_key, loop)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
