def row(boarding_pass):

    """
    >>> row('FBFBBFFRLR')
    44
    >>> row('BFFFBBFRRR')
    70
    >>> row('FFFBBBFRRR')
    14
    >>> row('BBFFBBFRLL')
    102
    """

    return int(boarding_pass[:7].replace('F', '0').replace('B', '1'), 2)

def column(boarding_pass):

    """
    >>> column('FBFBBFFRLR')
    5
    >>> column('BFFFBBFRRR')
    7
    >>> column('FFFBBBFRRR')
    7
    >>> column('BBFFBBFRLL')
    4
    """

    return int(boarding_pass[7:].replace('L', '0').replace('R', '1'), 2)

def seat_id(boarding_pass):

    """
    >>> seat_id('FBFBBFFRLR')
    357
    >>> seat_id('BFFFBBFRRR')
    567
    >>> seat_id('FFFBBBFRRR')
    119
    >>> seat_id('BBFFBBFRLL')
    820
    """

    return 8 * row(boarding_pass) + column(boarding_pass)

def highest_seat_id(boarding_passes):

    """
    >>> highest_seat_id('boarding_list.txt')
    820
    >>> highest_seat_id('adventofcode.input.txt')
    806
    """

    highest_id = 0
    with open(boarding_passes) as boarding_passes:
        for boarding_pass in boarding_passes:
            current_id = seat_id(boarding_pass.rstrip('\n'))
            highest_id = max(current_id, highest_id)

    return highest_id

if __name__ == '__main__':
    import doctest
    doctest.testmod()
