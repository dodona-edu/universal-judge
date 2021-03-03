class Ship:

    """
    >>> ship = Ship()
    >>> ship.move('F10')
    Ship(x=100, y=10, dx=10, dy=1)
    >>> ship.move('N3')
    Ship(x=100, y=10, dx=10, dy=4)
    >>> ship.move('F7')
    Ship(x=170, y=38, dx=10, dy=4)
    >>> ship.move('R90')
    Ship(x=170, y=38, dx=4, dy=-10)
    >>> ship.move('F11')
    Ship(x=214, y=-72, dx=4, dy=-10)
    >>> ship.distance()
    286
    """

    direction = {
        'N': (0, 1),
        'S': (0, -1),
        'E': (1, 0),
        'W': (-1, 0),
    }

    def __init__(self, x=0, y=0, dx=10, dy=1):

        # set the ship's initial position
        self.x, self.y = x, y

        # set the ship's initial direction
        self.dx, self.dy = dx, dy

    def __repr__(self):

        return f'Ship(x={self.x}, y={self.y}, dx={self.dx}, dy={self.dy})'

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
        elif action == 'F':
            self.x += self.dx * distance
            self.y += self.dy * distance
        else:
            dx, dy = Ship.direction[action]
            self.dx += dx * distance
            self.dy += dy * distance

        return self

def distance(filename):

    """
    >>> distance('instructions.txt')
    286
    >>> distance('adventofcode.input.txt')
    52069
    """

    ship = Ship()

    with open(filename) as instructions:
        for instruction in instructions:
            ship.move(instruction.rstrip('\n'))

    return ship.distance()

if __name__ == '__main__':
    import doctest
    doctest.testmod()