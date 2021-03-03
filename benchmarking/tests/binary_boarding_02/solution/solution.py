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

def missing_seat_id(boarding_passes):

    """
    >>> missing_seat_id('boarding_list.txt')
    658
    >>> missing_seat_id('adventofcode.input.txt')
    562
    """

    # compute list of seats
    seats = []
    with open(boarding_passes) as boarding_passes:
        for boarding_pass in boarding_passes:
            seats.append(seat_id(boarding_pass.rstrip('\n')))

    # find missing seat
    seats = sorted(seats)
    for index, seat in enumerate(seats):
        if seat + 1 != seats[index + 1]:
            return seat + 1

if __name__ == '__main__':
    import doctest
    doctest.testmod()
