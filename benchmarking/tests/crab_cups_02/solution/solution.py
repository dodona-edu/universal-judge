class Cups:

    """
    >>> cups = Cups([3, 8,  9,  1,  2,  5,  4,  6,  7])
    >>> cups
    Cups([3, 8, 9, 1, 2, 5, 4, 6, 7], current=3)
    >>> print(cups)
    (3) 8 9 1 2 5 4 6 7
    >>> cups.order
    '25467389'
    >>> cups.move()
    Cups([2, 8, 9, 1, 5, 4, 6, 7, 3], current=2)
    >>> cups.move()
    Cups([5, 4, 6, 7, 8, 9, 1, 3, 2], current=5)
    >>> cups.move()
    Cups([8, 9, 1, 3, 4, 6, 7, 2, 5], current=8)
    >>> cups.moves(7).order
    '92658374'
    >>> cups.moves(90).order
    '67384529'
    """

    def __init__(self, cups, current=None):

        # remember the current cup
        # NOTE: first cup is the default current cup
        # NOTE: all values are lowered by one to make the modulo operator work
        self.current = (cups[0] if current is None else current)

        # determine the length of the circular list
        self.length = len(cups)

        # dictionary maps elements to their successor in the circular linked list
        self._cups = {cups[-1]:cups[0]}
        for previous, next in zip(cups, cups[1:]):
            self._cups[previous] = next

    def __len__(self):

        return self.length

    def __repr__(self):

        return f'Cups([{", ".join(str(cup) for cup in self.cups())}], current={self.current})'

    def __str__(self):

        return ' '.join(
            f'{cup}' if cup != self.current else f'({cup})'
            for cup in self.cups()
        )

    def get_successor(self, cup):

        return self._cups[cup]

    def set_successor(self, cup, cup2):

        self._cups[cup] = cup2

    def cups(self, start=None):

        cup = self.current if start is None else start
        for _ in range(self.length):
            yield cup
            cup = self.get_successor(cup)

    @property
    def order(self):

        return ''.join(str(cup) for cup in self.cups(start=1))[1:]

    def move(self):

        # determine the length of the cups
        length = len(self)

        # remove the three cups immediately clockwise of current cup
        picked_up = [self.get_successor(self.current)]
        picked_up.append(self.get_successor(picked_up[-1]))
        picked_up.append(self.get_successor(picked_up[-1]))
        self.set_successor(self.current, self.get_successor(picked_up[-1]))

        # determine position of destination cup
        destination = (self.current - 2) % length + 1
        while destination in picked_up:
            destination = (destination - 2) % length + 1

        # insert the removed cups at the destination
        self.set_successor(picked_up[-1], self.get_successor(destination))
        self.set_successor(destination, picked_up[0])

        # select a new current cup
        self.current = self.get_successor(self.current)

        return self

    def moves(self, count):

        for _ in range(count):
            self.move()

        return self

def product(cups, total, moves):

    """
    >>> product(389125467, 9, 0)
    10
    >>> product(389125467, 9, 1)
    20
    >>> product(389125467, 9, 2)
    6
    >>> product(389125467, 9, 3)
    12
    >>> product(389125467, 9, 4)
    6
    >>> product(389125467, 9, 5)
    18
    >>> product(389125467, 9, 6)
    27
    >>> product(389125467, 9, 7)
    18
    >>> product(389125467, 9, 8)
    40
    >>> product(389125467, 9, 9)
    24
    >>> product(389125467, 9, 10)
    18
    >>> product(389125467, 9, 100)
    42
    >>> product(389125467, 10 ** 6, 10 ** 7)
    149245887792

    >>> product(952316487, 10 ** 6, 10 ** 7)
    363807398885
    """

    # initialize the game
    game = Cups(
        [int(cup) for cup in str(cups)] +
        list(range(len(str(cups)) + 1, total + 1))
    )

    # perform the given number of moves
    game.moves(moves)

    # compute product
    factor1 = game.get_successor(1)
    factor2 = game.get_successor(factor1)
    return factor1 * factor2

if __name__ == '__main__':
    import doctest
    doctest.testmod()