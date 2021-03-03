class RFID:

    """
    >>> rfid_7 = RFID(7)
    >>> rfid_7.loop(5764801)
    8
    >>> rfid_7.loop(17807724)
    11

    >>> RFID(5764801).value(11)
    14897079
    >>> RFID(17807724).value(8)
    14897079
    """

    def __init__(self, subject_number):

        # fixed subject number
        self._subject_number = subject_number

        # initialize loop size and value
        self._loop = 0
        self._value = 1

        # remember previous loop sizes and values
        self._value2loop = {self._value: self._loop}
        self._loop2value = {self._loop: self._value}

    def extend(self):

        self._loop += 1
        self._value = (self._value * self._subject_number) % 20201227
        self._value2loop[self._value] = self._loop
        self._loop2value[self._loop] = self._value

    def loop(self, value):

        # check if value has been seen before
        if value in self._value2loop:
            return self._value2loop[value]

        # continue looping until value is found
        while value != self._value:
            self.extend()

        return self._loop

    def value(self, loop):

        # check if loop has already been computed
        if loop <= self._loop:
            return self._loop2value[loop]

        # continue looping until value is found
        while loop != self._loop:
            self.extend()

        return self._value

def loop_size(public_key, subject_number):

    """
    >>> loop_size(5764801, 7)
    8
    >>> loop_size(17807724, 7)
    11
    """

    return RFID(subject_number).loop(public_key)

def transform(subject_number, loop_size):

    """
    >>> transform(17807724, 8)
    14897079
    >>> transform(5764801, 11)
    14897079
    """

    return RFID(subject_number).value(loop_size)

def encryption_key(card_key, door_key, subject_number):

    """
    >>> encryption_key(5764801, 17807724, 7)
    14897079
    >>> encryption_key(15327461, 11869933, 7)
    14353690
    >>> encryption_key(9093927, 11001876, 7)
    12227206
    """

    return RFID(door_key).value(RFID(subject_number).loop(card_key))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
