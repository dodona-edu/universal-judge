def waiting_time(earliest_departure, bus_id):

    """
    >>> waiting_time(939, 7)
    6
    >>> waiting_time(939, 13)
    10
    >>> waiting_time(939, 19)
    11
    >>> waiting_time(939, 31)
    22
    >>> waiting_time(939, 59)
    5
    """

    departure = (earliest_departure // bus_id) * bus_id
    if departure < earliest_departure:
        departure += bus_id

    return departure - earliest_departure

def departure(earliest_departure, buses):

    """
    >>> departure(939, '7,13,x,x,59,x,31,19')
    295
    >>> departure(1008141, '17,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,23,x,x,x,x,x,x,x,787,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29')
    4722
    """

    travel_bus = None
    travel_wait = None
    for bus_id in buses.split(','):
        if bus_id != 'x':
            bus = int(bus_id)
            wait = waiting_time(earliest_departure, bus)
            if travel_wait is None or wait < travel_wait:
                travel_bus = bus
                travel_wait = wait

    return travel_bus * travel_wait

if __name__ == '__main__':
    import doctest
    doctest.testmod()
