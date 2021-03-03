class Ship:

    """
    >>> ship = Ship()
    >>> ship.move('F10')
    Ship(10, 0)
    >>> ship.move('N3')
    Ship(10, 3)
    >>> ship.move('F7')
    Ship(17, 3)
    >>> ship.move('R90')
    Ship(17, 3)
    >>> ship.move('F11')
    Ship(17, -8)
    >>> ship.distance()
    25
    """

    direction = {
        'N': (0, 1),
        'S': (0, -1),
        'E': (1, 0),
        'W': (-1, 0),
    }

    def __init__(self, x=0, y=0, dx=1, dy=0):

        # set the ship's initial position
        self.x, self.y = x, y

        # set the ship's initial direction
        self.dx, self.dy = dx, dy

    def __repr__(self):

        return f'Ship({self.x}, {self.y})'

    def distance(self):

        return abs(self.x) + abs(self.y)

    def turn_left(self, degrees):

        for _ in range(degrees // 90):
            self.dx, self.dy = -self.dy, self.dx

    def turn_right(self, degrees):

        for _ in range(degrees // 90):
            self.dx, self.dy = self.dy, -self.dx

    def move(self, instruction):

        # parse the instruction
        action, distance = instruction[0], int(instruction[1:])

        if action == 'L':
            self.turn_left(distance)
        elif action == 'R':
            self.turn_right(distance)
        else:

            # ship moves without changing direction
            if action == 'F':
                dx, dy = self.dx, self.dy
            else:
                dx, dy = Ship.direction[action]

            self.x += dx * distance
            self.y += dy * distance

        return self

def distance(filename):

    """
    >>> distance('instructions.txt')
    25
    >>> distance('adventofcode.input.txt')
    582
    """

    ship = Ship()

    with open(filename) as instructions:
        for instruction in instructions:
            ship.move(instruction.rstrip('\n'))

    return ship.distance()

if __name__ == '__main__':
    import doctest
    doctest.testmod()